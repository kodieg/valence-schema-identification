package kodie.phd

import experimental.ArgumentWithProb
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.formats.Word2Vec
import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty.FrameChooserCascade
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty.WalentyExamplesUtils
import kodie.phd.walenty._
import main.pas.EvaluateFrameMatchingWith
import org.apache.commons.io.FileUtils
import org.apache.spark.{SparkConf, SparkContext}
import phd.sr.counts.{BaseCounts, VerbsCounts}
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.pd._
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.Try

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.apache.spark.rdd.RDD

sealed trait Algo {
  def toAlgoString: String
}
case class Simple(text: String) extends Algo {
  def toAlgoString = text
}
case class Vote(minVotes: Double, algos: Seq[String]) extends Algo {
  def toAlgoString = s"vote:${"%.1f".format(minVotes)}:${algos.mkString(";")}"
}
case class Solution(algos: Seq[Algo]) {
  override def toString = algos.map(_.toAlgoString).mkString(",")
}
object Solution {
  def fromString(s: String) = {
    val algos = s.split(',').map {
      case s if s.startsWith("vote") =>
        val parts = s.split("\\Q:\\E", 3)
        Vote(parts(1).toDouble, parts(2).split(';'))
      case s => Simple(s)
    }.toSeq

    Solution(algos)
  }
}

class SimulatedAnnealingSolutionFinder(availableAlgos: Seq[String], evalFunc: String => (Double, Map[Int, Int], Double), maxAlgos: Int = 10, voteRatio: Double = 0.3, minResetSteps: Int = 20) {

  /**
   * normalization means here removing all classifiers from solution that had not produced any output
   * thus solution is simplified!
   * @param solution solutions to be normalized
   * @param counts   counts classifier index (in solution.algos) -> number of examples that had been answered
   * @return
   */
  def normalizeSolution(solution: Solution, counts: Map[Int, Int]) : Solution = {
    val indexes = counts.iterator.filter(_._2 > 0).map(_._1).toSet
    solution.copy(
      algos = solution.algos.iterator.zipWithIndex.filter(p => indexes(p._2)).map(_._1).toSeq
    )
  }

  def compute(maxSteps: Int, initialTemperature: Double, coolingRate: Double, initialSolution: Solution) = {
    var step = 0
    var temperature = initialTemperature
    val currentUnnorm = initialSolution
    var (energy, currentClassifierCounts, realAcc) = evaluate(currentUnnorm)
    var current = normalizeSolution(currentUnnorm, currentClassifierCounts)

    var bestSolution = current
    var bestEnergy = energy
    var bestRealAcc = realAcc
    var lastResetStep = 0
    var bestStep = step

    while (step < maxSteps) {
      step += 1
      val newSolutionUnnorm = neighbour(current)
      val (newEnergy, newClassifierCounts, newRealAcc) = evaluate(newSolutionUnnorm)
      val newSolution = normalizeSolution(newSolutionUnnorm, newClassifierCounts)

      if (acceptanceProbability(energy, newEnergy, temperature) >= Random.nextDouble()) {
        current = newSolution
        energy = newEnergy
        realAcc = newRealAcc
      }

      if (newEnergy < bestEnergy) {
        bestSolution = newSolution
        bestEnergy = newEnergy
        bestStep = step
        lastResetStep = step
        bestRealAcc = newRealAcc
      }

      if ((step - lastResetStep > minResetSteps) && (energy > bestEnergy)) {
        println(s"RESET TO BEST: $step")
        current = bestSolution
        energy = bestEnergy
        lastResetStep = step

      }

      println(s"STEP: $step / $maxSteps; current temp: $temperature; best: $bestEnergy, bestAcc: $bestRealAcc, current: $energy, new $newEnergy, best solution: ${bestSolution.toString}, newSolution, ${newSolution.toString} (acc: $newRealAcc)")
      temperature = (coolingRate) * temperature
    }

    (bestSolution, bestEnergy, bestRealAcc)
  }

  def neighbour(s: Solution, retriesLeft: Int = 3): Solution = {
    val numAlgos = s.algos.size
    val canAdd = numAlgos < maxAlgos

    val action = Random.nextInt((if (canAdd) numAlgos + 5 else numAlgos) - 1) + 1  // keep first max:false! always

    val randomAlgo = pickRandomAlgo()

    if (action >= numAlgos) {
      val b = s.algos.toBuffer
      val pos = Random.nextInt(s.algos.size) + 1
      // try not to put algo that is already earlier to the solution (as it'll not do any good)
      if (retriesLeft <= 0 || !s.algos.view.slice(0, pos+1).map(_.toAlgoString).contains(randomAlgo.toAlgoString)) {
        b.insert(pos, randomAlgo) // purposely not allowing to insert at 0 here
        Solution(b.toSeq)
      } else {
        neighbour(s, retriesLeft - 1)
      }
    } else {
      if (retriesLeft <= 0 || !s.algos.view.slice(0, action).map(_.toAlgoString).contains(randomAlgo.toAlgoString)) {
        Solution(s.algos.updated(action, randomAlgo))
      } else {
        neighbour(s, retriesLeft - 1)
      }
    }
  }

  def pickRandomAlgo(): Algo = {
    if (Random.nextDouble() < voteRatio) {
      val numAlgos = Random.nextInt(4) + 2
      val minVotes = Random.nextDouble() * (numAlgos - 1.1) + 0.1
      // sorted as ordering doesn't matches, to make comparing more fair!
      // though probability of randomly choising two exactly the same vote algorithms is minial
      // rather!
      Vote(minVotes, Random.shuffle(availableAlgos).take(numAlgos).sorted)
    } else {
      Simple(availableAlgos(Random.nextInt(availableAlgos.size)))
    }
  }

  def evaluate(s: Solution): (Double, Map[Int, Int], Double) = {
    val (accuracy, classifierCounts, realAcc) = evalFunc(s.toString)
    (1.0 - accuracy, classifierCounts, realAcc)  // eval func should be correctness! though energy should be decreasing!
  }

  def acceptanceProbability(energy: Double, newEnegry: Double, temperature: Double) = {
    val diff = newEnegry - energy
    if (diff < 0 ) 1.0 else math.exp(-diff/temperature)
  }
}

object SAFindBestSolution extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }


  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs0)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }

  val walenty = NewWalentyParser()


  val wordnet = manager.computeResource[PLWordnet]
  @transient val chooserFactory = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent)
  //.fromString(algosStr)

  //val inputPath = /*args.lift(1).getOrElse(*/"data/pantera-new-multi-with-cp-heur-walentyExamples-with-args-and-ann.obj"//)
  //val inputPath = path.getOrElse("data/pantera-new-multi-with-cp-heur-walentyExamples-with-args-and-ann.obj")

  val config = new SparkConf().setMaster("local[40]").setAppName("test-app").set("spark.executor.memory", "30g")
  implicit val sc = new SparkContext(config)
  val scorer = new SparkTaskScorer(path, sc, walenty)

  val scoresCache = scala.collection.mutable.Map[String, (Double, Map[Int, Int], Double)]()
  //val queue = scala.collection.mutable.Queue[String]()

  def scoreWithCaching(algo: String) = {
    if (scoresCache.contains(algo)) {
      println(s"CACHE: Using cache for $algo --> ${scoresCache(algo)} --> ${scoresCache.size}")
      scoresCache(algo)
    }
    val chooser = chooserFactory.fromString(algo)
    val result = scorer.scoreMe(chooser)


    // TODO: cache clearing if needed
    //if (scoresCache.size > 10000)

    //queue.enqueue(algo)
    scoresCache.put(algo, result)
    result
  }


  val algos = Seq(
    "max:false", "maxNoXP:false", "max:true", "maxNoXP:true", "max:false:true", "max:true:true", "lex:false", "lex:true",
    "bayes:false:0.4:50.0:false", "bayes:false:0.3:50.0:false", "bayes:false:0.2:50.0:false", "bayes:false:0.2:20.0:false", "bayes:false:0.1:20.0:false", "bayes:false:0.1:10.0:false",
    "bayes:true:0.4:50.0:false", "bayes:true:0.3:50.0:false", "bayes:true:0.2:50.0:false", "bayes:true:0.2:20.0:false", "bayes:true:0.1:20.0:false", "bayes:true:0.1:10.0:false",
    "negbayes:true:0.4:50.0:false", "negbayes:true:0.3:50.0:false", "negbayes:true:0.2:50.0:false", "negbayes:true:0.2:20.0:false", "negbayes:true:0.1:20.0:false", "negbayes:true:0.1:10.0:false",
    "vec:logreg:0.5:100.0", "vec:logreg:0.3:100.0", "vec:logreg:0.2:100.0", "vec:logreg:0.1:100.0",
    "vec:logreg:0.3:20.0", "vec:logreg:0.2:20.0", "vec:logreg:0.1:20.0",
    "vec:logreg:0.3:20.0:true", "vec:logreg:0.2:20.0:true", "vec:logreg:0.1:20.0:true",
    "http:0.1:0.5:cache", "http:0.3:0.5:cache", "http:0.1:0.6:cache", "http:0.3:0.6:cache", "http:0.1:0.7:cache", "http:0.3:0.7:cache", "http:0.1:0.8:cache", "http:0.3:0.8:cache",
    "sr:splitsum:false:0.3:false","sr:splitsum:false:0.2:false","sr:splitsum:false:0.1:false",
    "sr:splitsum:true:0.3:false","sr:splitsum:true:0.2:false","sr:splitsum:true:0.1:false",
    "sr:clark:false:0.3:false","sr:clark:false:0.2:false","sr:clark:false:0.1:false",
    "sr:clark:true:0.3:false","sr:clark:true:0.2:false","sr:clark:true:0.1:false",
    "sr:selassoc:false:0.3:false","sr:selassoc:false:0.2:false","sr:selassoc:false:0.1:false",
    "sr:selassoc:true:0.3:false","sr:selassoc:true:0.2:false","sr:selassoc:true:0.1:false",
    "srone:splitsum:20:90","srone:splitsum:20:20","srone:splitsum:30:30","srone:splitsum:30:50", "srone:splitsum:20:50",
    "srone:splitsum:10:90", "srone:splitsum:90:90", "srone:splitsum:10:10",
    "srone:splitsum:20:90:true","srone:splitsum:20:20:true","srone:splitsum:30:30:true","srone:splitsum:30:50:true", "srone:splitsum:20:50:true",
    "srone:splitsum:10:90:true", "srone:splitsum:90:90:true", "srone:splitsum:10:10:true"
  )
  val sa = new SimulatedAnnealingSolutionFinder(algos, scoreWithCaching _, 15, 0.25, 100)

  val initSolStr = "max:false,lex:true,sr:clark:true:0.3:false,maxNoXP:true,bayes:false:0.3:50.0:false,srone:splitsum:90:90,sr:splitsum:false:0.3:false,vec:logreg:0.2:20.0,vote:1.0:http:0.1:0.7:cache;sr:splitsum:true:0.3:false,vote:0.1:bayes:false:0.2:20.0:false;sr:clark:false:0.1:false;sr:splitsum:false:0.3:false,max:true:true,vote:0.7:bayes:false:0.1:10.0:false;sr:selassoc:false:0.2:false"
  val initSol = Solution.fromString(initSolStr)
  //val initSol = Solution(Seq(Simple("max:false"), Simple("lex:false"), Simple("max:true"), Simple("http:0.3:0.6:cache"), Simple("bayes:true:0.3:50:false"), Simple("sr:splitsum:true:0.3:false"), Simple("vec:logreg:0.3:20.0")))

  print(s"Initial solution $initSol")
  val result = sa.compute(10000, 10000, 0.998, initSol)
  println(s"FINAL RESULT! ${result._1} -- acc: ${1.0 - result._2}}, realAcc: ${result._3}")
}

class SparkTaskScorer(path: Option[String], sc: SparkContext, walenty: NewWalentyParser, optRandDelta: Boolean = true) extends Serializable {

  val multiChooser = new MultiFrameChooser(walenty)

  val inputPath = path.getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-L1-c-1.0-minFreq-1-label-P/walenty-examples-keep-labels-2-keep-thr-0.9-usecp-true.obj")
  val allData = sc.objectFile[((Sentence, Seq[ArrayBuffer[ArgumentWithProb]]), Map[String, String])](inputPath).sample(false, 0.25)

  val dataRaw = allData.filter { case ((sentence, args), walentyAnn) =>
    val pos = WalentyExamplesUtils.argsToWalentyPositions(walentyAnn.getOrElse("wybor argumentow", ""))
    val goldPredicate = walentyAnn("haslo")
    val goldFrame = walentyAnn("ramka")
    val hasSieArgument = goldFrame.startsWith("się")
    val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"

    val frames = walenty.framesFor(goldPredicate, aspect, hasSieArgument)

    // TODO: mozna tu nie wycinac X + lex...
    val matchingFrames = frames.filter(f => WalentyExamplesUtils.frameContainsAllPositions(f, pos))
    val isAmbiguous = matchingFrames.size > 1
    if (isAmbiguous) {
      println(s"Removing sentence: ${sentence.text} -- ${matchingFrames.map(_.frameText).mkString("@")}")
    }
    val newCorpusAmbig = walentyAnn.get("supercomment").orElse(walentyAnn.get("comment1").filter(_.toLowerCase.contains("niejedn")).orElse(walentyAnn.get("comment2"))).exists(_.toLowerCase.contains("niejedn"))
    /*!isAmbiguous &&*/ !newCorpusAmbig && !goldFrame.contains("{or}") // !walentyAnn.getOrElse("wybor argumentow", "").contains("{or}")
  }

  println(s"Input path: $inputPath")

  val data = dataRaw.filter {
    case ((sentence, args), walentyAnn) =>
      val goldFrame = walentyAnn("ramka")
      EvaluateFrameMatchingWith.isVerbFrame(goldFrame)
  }.cache()
  /*allData.take(100).foreach { case ((sent, args), _) =>
    println(args)

}*/

  val total = data.count()


  val dataWithPredicates = data.filter {
    case ((sentence, args), walentyAnn) =>

      val goldPredicate = walentyAnn("haslo")
      val goldFrame = walentyAnn("ramka")

      val maybePredicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
      sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
      maybePredicateIndex != None // && ":_::imperf:subj{np(str)}+{np(dat)}+{or}" == goldFrame && goldPredicate == "wieszczyć"
  }.collect().par
  //dataWithPredicates.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool())

  sc.stop()




  def scoreMe(chooser: FrameChooserCascade): (Double, Map[Int, Int], Double) = {
// todo par?
    val results = dataWithPredicates.map {
      case input@((sentence, args), walentyAnn) =>
        val goldPredicate = walentyAnn("haslo")
        val goldFrame = walentyAnn("ramka")

        // Filtered out cases where cannot find predicate.
        val predicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2).get

        val predicateIndexes = args.flatten.map(_.predicate.left).toSet.toSeq.sorted
        val leftNeighbour =  Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
        val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption


        // Stealed arguments -- now - use order of heuristics? frst extendedArgs or first SR!

        val leftNeighIndex = leftNeighbour.getOrElse(0)
        val rightNeighIndex = rightNeighbour.getOrElse(sentence.words.size)
        //val extendedPredicates = Seq(predicateIndex) ++ leftNeighbour.toSeq ++ rightNeighbour.toSeq


        // Stealed arguments -- now - use order of heuristics? frst extendedArgs or first SR!
        val extendedArgs = args.zipWithIndex.map(pair => pair._1.filter { arg =>
          arg.predicate.left == predicateIndex || (pair._2 >= leftNeighIndex && pair._2 <= rightNeighIndex) } )

        /*if (sentence.words.exists(_.orth == "powabną") && sentence.words.exists(_.orth == "damą")) {
          println("XXXXXXXXXXXXXXXXXX")
          println(args)
          println(extendedArgs)
        }*/
        /*
        val extendedArgs = args.zipWithIndex.map(pair => pair._1.filter { arg =>
          arg.predicate.left == predicateIndex ||
            leftNeighbour.map(left => arg.predicate.left == left && pair._2 > left).getOrElse(false) ||
            rightNeighbour.map(right => arg.predicate.left == right && pair._2 < right).getOrElse(false)
        } )
        */
        // old definition (a little bit higher recall , less precision) was
        //       val extendedPredicates = Seq(predicateIndex) ++ leftNeighbour.toSeq ++ rightNeighbour.toSeq

        // val extendedArgs = args.map(_.filter(arg => extendedPredicates.contains(arg.predicate.left)))

        val predicateArgs = args.map(_.filter(p => p.predicate.left == predicateIndex || p.predicate.left == -1))
        val predicate = sentence.words(predicateIndex)
        val base = predicate.base
        val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"

        //implicit def argumentWithProbToArgument(arg: ArgumentWithProb) = Argument(arg.argumentType, arg.predicate, arg.semanticHead)


        val outputTriple@(predicted, debugInfo, classifierIndex) = chooser.chooseFrame(base, aspect, sentence.words, predicateArgs, extendedArgs)

        val output = ((predicted, debugInfo), classifierIndex)

        val result = predicted match {
          case Some(chooserResult) if EvaluateFrameMatchingWith.comparableFormat(chooserResult.frame.frameText) == EvaluateFrameMatchingWith.comparableFormat(goldFrame) => 1.0
          case Some(_) => 0.0
          case None if optRandDelta =>
            val frames = multiChooser.chooseMatchingFrames(base, aspect, sentence.words, predicateArgs)
            val vals = frames.collect {
              case (Some(result), _) => result.argumentsMatching.count(_.isDefined)
            }

            if (vals.nonEmpty) {
              val maxFCnt = vals.max
              val maxFrames = vals.count(_ == maxFCnt).toDouble
              val hasCorrect = frames.exists {
                case (Some(result), _) if result.argumentsMatching.count(_.isDefined) == maxFCnt => EvaluateFrameMatchingWith.comparableFormat(result.frame.frameText) == EvaluateFrameMatchingWith.comparableFormat(goldFrame)
                case _ => false
              }
              if (hasCorrect) {
                1.0 / maxFrames
              } else {
                0.0
              }
            } else {
              0.0
            }
          case None => 0.0
        }

        (result, ((sentence, predicateArgs), walentyAnn), output)
    }/*.sample(false, 0.001)*/

    val realCorrect = results.count(_._1 == 1.0)
    val correct = results.map(_._1).sum//.count
    val countsByIndex = results.groupBy(_._3._2).mapValues(_.count(_._3._1._1.isDefined)).map(identity _).seq

    (correct.toDouble / total, Map[Int, Int]() ++ countsByIndex, realCorrect.toDouble / total)

  }


}