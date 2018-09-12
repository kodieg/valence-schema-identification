package main.pas

import java.io.PrintWriter

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
import scala.io.Source
import scala.util.Try

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.apache.spark.rdd.RDD


object MultiFrameEval extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }

  val (newArgs1, algoFilePath) = if (newArgs0(0) == "--algo-file") {
    (newArgs0.drop(2), newArgs0(1))
  } else {
    (newArgs0, "algo.txt")
  }
  val (newArgs2, outputPrefix) = if (newArgs1(0) == "--output") {
    (newArgs1.drop(2), newArgs1(1))
  } else {
    (newArgs1, "algo.txt")
  }


  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs2)
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

  //val queue = scala.collection.mutable.Queue[String]()

  Source.fromFile(algoFilePath).getLines.foreach { line =>
    val algoStr = line.trim()
    val output = s"${outputPrefix}-${algoStr}.txt"


    val chooser = chooserFactory.fromString(algoStr)
    val result = scorer.scoreMe(chooser)

    val out = new PrintWriter(output)

    out.println(s"Evaluation: $algoStr")
    out.println(s"Accuracy: ${result.accuracy}")
    out.println(s"Precision: ${result.precision}")
    out.println(s"Ambig precision: ${result.ambigPrecision}")
    out.println(s"Correct: ${result.correct}")
    out.println(s"Total not none: ${result.notNone}")
    out.println(s"Total: ${result.total}")
    out.println(s"Errors: ${result.errors}\n")

    for ((a, i) <- algoStr.split(",").zipWithIndex) {
      val clsResult = result.classifiers.get(i)
      val rat = clsResult.map(_._1).getOrElse(0).toDouble/clsResult.map(_._2).getOrElse(1)
      val rand = clsResult.map(_._3).getOrElse(0.0).toDouble/clsResult.map(_._2).getOrElse(1)
      out.println(s"Classifier $i ($a): ${clsResult.map(_._1).getOrElse(0)}/${clsResult.map(_._2).getOrElse(0)}, prec: $rat [rand: ${rand}]")
    }
    out.close()
  }

}

case class SparkTaskScorerResult(accuracy: Double, precision: Double, ambigPrecision: Double, correct: Int, notNone: Long, total: Long, errors: Long, classifiers: Map[Int, (Int, Int, Double)])
class SparkTaskScorer(path: Option[String], sc: SparkContext, walenty: NewWalentyParser) extends Serializable {



  val inputPath = path.getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-L1-c-1.0-minFreq-1-label-P/walenty-examples-keep-labels-2-keep-thr-0.9-usecp-true.obj")
  val allData = sc.objectFile[((Sentence, Seq[ArrayBuffer[ArgumentWithProb]]), Map[String, String])](inputPath)//.sample(false, 0.1)

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

  val errors = total - dataWithPredicates.size
  //dataWithPredicates.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool())

  sc.stop()


  def scoreMe(chooser: FrameChooserCascade): SparkTaskScorerResult = {
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
          case None => false
          case Some(chooserResult) => EvaluateFrameMatchingWith.comparableFormat(chooserResult.frame.frameText) == EvaluateFrameMatchingWith.comparableFormat(goldFrame)
        }

        (result, ((sentence, predicateArgs), walentyAnn), output)
    }/*.sample(false, 0.001)*/

    val correct = results.count(_._1)//.count
    val notNone = results.count(_._3._1._1.isDefined)
    val easyCases = results.count(_._3._1._1.map(_.candidatesCount == 1).getOrElse(false))
    val countsByIndex = results.groupBy(_._3._2).mapValues {
      x =>
        val cor = x.count(_._1)
        val tot = x.count(_._3._1._1.isDefined)
        val errs = x.view.filter(_._3._1._1.isDefined).map { xx =>
          // filtering out swapper events for rand alg calculations!
          val maxFrames = xx._3._1._2.events.filter(c => c.frameSwapper == c.frame.frameText)

          val correctInMax = maxFrames.exists(e => EvaluateFrameMatchingWith.comparableFormat(e.frameString) == EvaluateFrameMatchingWith.comparableFormat(xx._2._2.getOrElse("ramka",  "???")))
          val avgr = if (correctInMax) {
            1.0 / maxFrames.size
          } else {
            0.0
          }
          avgr
        }.sum

        (cor, tot, errs)
    }.map(identity _).seq

    SparkTaskScorerResult(correct.toDouble / total, correct.toDouble/notNone,  (correct - easyCases).toDouble / (notNone - easyCases), correct, notNone, total, errors, Map[Int, (Int, Int, Double)]() ++ countsByIndex)

  }


}