package kodie.phd.walenty

import java.io.FileInputStream

import kodie.phd.classify.{SRPercentilesProvider, SRBoundariesProvider}
import kodie.phd.formats.Word2Vec
import kodie.phd.tools.wordnet

import scala.concurrent.forkjoin.ForkJoinPool

import experimental.{SchemaFeatureExtractor, ArgumentWithProb}
import kodie.core.soa.ServiceManager
import kodie.phd.skladnica.types.Word
import phd.sr.counts.BaseCounts
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.SplitSumCounts
import phd.sr.scorer.pd.{ScoreBasedVerbClassifier, SelectionalAssociationClassifier, SplitSumVerbClassifier, ClarkWeirVerbClassifier}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}
import kodie.phd.tools.{wordnet => wn}

object FrameChooserCascade {
  type Chooser = (String, String, Seq[Word], Seq[ArrayBuffer[ArgumentWithProb]], Seq[ArrayBuffer[ArgumentWithProb]]) => (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo)
}

class FrameChooserCascade(val functions: Seq[FrameChooserCascade.Chooser]) extends Serializable {
  // chooses frame and returns Option[frame], debugInfo, whichClassifierWasLastUsed
  def chooseFrame(base: String, aspect: String, words: Seq[Word], skladnicaArgs: Seq[ArrayBuffer[ArgumentWithProb]], extendedSkladnicaArgs: Seq[ArrayBuffer[ArgumentWithProb]]): (Option[kodie.phd.walenty.MaximalFrameChooserResult], kodie.phd.walenty.MaximalFrameChooserDebugInfo, Int) = {
    val results = functions.view.zipWithIndex.map { case(f, index) =>
      val (a, b) = f(base, aspect, words, skladnicaArgs, extendedSkladnicaArgs)
      (a, b, index)
    }
    results.find(_._1.isDefined).getOrElse(results.last)
  }
}


class FrameChooserCascadeFactory(@transient manager: ServiceManager, path: String) extends Serializable {
  @transient val walenty = NewWalentyParser()
  @transient val boundaries = new SRBoundariesProvider(manager, path)
  @transient val percentiles = new SRPercentilesProvider(manager, path)
  @transient val index = manager.computeResource[WordsIndex]

    @transient val in = new FileInputStream("data/300m.bin")
  @transient lazy val baseCounts = manager.computeResource[BaseCounts]

  @transient val plWordnet = wn.PLWordnet.load()

  @transient val lexToDomains = plWordnet.units.values.map(lu => lu.name -> lu.domain).groupBy(_._1).mapValues(_.map(_._2).toArray)

  @transient val word2vec = Word2Vec.readBinary(in)
  @transient lazy val schemaVecs = SchemaVectorsProviders.calculate(path, manager.computeResource[PredicateGroupDataset], index, word2vec)
  @transient lazy val logisticRegClassifiers = SchemaLogRegVectorsProviders.calculate(path, manager.computeResource[PredicateGroupDataset], index, word2vec)
  @transient lazy val frameSwapperBuilder = new FrameSwapperBuilder(walenty, index, baseCounts.predicateCounts  )
  @transient lazy val featuresExtractor = new SchemaFeatureExtractor(lexToDomains, word2vec, index, new CosineSimilarity(schemaVecs), new LogRegressionSimilarity(logisticRegClassifiers), manager.computeResource[SplitSumVerbClassifier], manager.computeResource[ClarkWeirVerbClassifier])

  def fromString(algos: String): FrameChooserCascade = {
    val classifiers = algos.split(",").map { algoWithParams =>
      val all = algoWithParams.split(":")
      val params = all.drop(1)
      val algo = all(0)
      println(s"Algo $algo -- params: ${params.toList}")
      algo match {
        case "max" => maxFrame(params(0).toLowerCase() == "true", params.lift(1).exists(_ == "true"))
        case "maxNoXP" => maxFrame(params(0).toLowerCase() == "true", params.lift(1).exists(_ == "true"), true, params.lift(2).exists(_ == "true"))
        case "lex" => lexFrame(params(0).toLowerCase() == "true")
        case "sr"  => srVote(params(0).toLowerCase(), params(1).toLowerCase() == "true", params(2).toDouble, params(3).toLowerCase() == "true")
        case "srone" => srVoteOne(params(0).toLowerCase(), params(1).toInt, params(2).toInt, params.lift(3).exists(_.toLowerCase() == "true"))
        case "srperc" => srPerc(params(0).toLowerCase(), params(1).toDouble)
        case "bayes" => bayes(params(0).toLowerCase() == "true", params(1).toDouble, params(2).toDouble, params(3).toLowerCase() == "true", params.lift(4).map(_.toLowerCase()).getOrElse("true") == "true" )
        case "negbayes" => negbayes(params(0).toLowerCase() == "true", params(1).toDouble, params(2).toDouble, params(3).toLowerCase() == "true")
        case "vec" => vecClassifier(params(0).toLowerCase(), params.lift(1).map(_.toDouble).getOrElse(0.0), params.lift(2).map(_.toDouble).getOrElse(0.0), params.lift(3).exists(_ == "true"))
        case "http" => httpClassifier(params(0).toDouble, params(1).toDouble, params.lift(2).exists(_.toLowerCase == "cache"))
        case "vote" =>
          println("VOTE BEGIN:")
          val minVotes = params(0).toDouble
          val clsWithWeights = params.drop(1).mkString(":").split(";").map { entry =>
            val data = entry.split("@")
            val spec = data(0)
            val weight = data.lift(1).map(_.toDouble).getOrElse(1.0)
            // we know that there is only one entry there!
            fromString(spec).functions.head -> weight
          }
          println("VOTE END")
          val vote = new VoteFrameChooser(clsWithWeights, minVotes)
          vote.chooseFrame _
      }
    }.toSeq
    new FrameChooserCascade(classifiers)
  }

  def aspectSwapper(a: String) = a match {
    case "imperf" => "perf"
    case "perf" => "imperf"
    case a => a
  }

  def maxFrame(useExtendedArgs: Boolean, swapAspect: Boolean, noXP: Boolean = false, keepFull: Boolean = false) = {
    val maxFrameChooser = if (noXP) {
      new MaximalFrameChooserButIgnoringXP(walenty, new AndFilterer(InfFilterer, LexicalizedFilterer), keepFull)
    } else {
      new MaximalFrameChooser(walenty, new AndFilterer(InfFilterer, LexicalizedFilterer), keepFull)
    }
    println(s"MaxFrame: useExtendedArgs=$useExtendedArgs swapAspect=$swapAspect ignoringXP=$noXP")

    (predicateBase: String, aspectOriginal: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) => {
      val aspect = if (swapAspect) aspectSwapper(aspectOriginal) else aspectOriginal
      if (useExtendedArgs)
        maxFrameChooser.chooseMaximalFrame(predicateBase, aspect, words, extendedArgs)
      else
        maxFrameChooser.chooseMaximalFrame(predicateBase, aspect, words, skladnicaArguments)
    }
  }

  def lexFrame(useExtendedArgs: Boolean) = {
    val maxFrameChooser = new LexSchemaChooser(walenty)
    println(s"LexSchema: useExtendedArgs=$useExtendedArgs")

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) => {
      if (useExtendedArgs)
        maxFrameChooser.chooseFrame(predicateBase, aspect, words, extendedArgs)
      else
        maxFrameChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments)
    }
  }

  def httpClassifier(thr: Double, minProb: Double, useCache: Boolean) = {
    val httpCls = new ScikitHTTPFrameChooser(walenty, featuresExtractor, frameSwapperBuilder, useCache = useCache)
    println(s"HTTP: thr=$thr minProb=$minProb useCache=$useCache")

    (predicateBase: String, aspectOriginal: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) => {
      httpCls.chooseFrame(predicateBase, aspectOriginal, words, skladnicaArguments, false, thr, minProb)
    }
  }

  // TODO: support for frame swapper/extended args/some params!
  def vecClassifier(algo: String, thr: Double, minFrameCount:Double, useFrameSwapper: Boolean) = {
    val baseCounts = manager.computeResource[BaseCounts]

    val rand = (algo == "random")
    println(s"VEC chooser algo=$algo, thr=$thr   minCnt$minFrameCount")

    val similarity = if (algo == "logreg") {
      new LogRegressionSimilarity(logisticRegClassifiers)
    } else { // TODO: for random no one cares :)
      new CosineSimilarity(schemaVecs)
    }


    val vecChooser = new SchemaVecChooser(walenty, baseCounts, index, similarity, word2vec, frameSwapperBuilder)

    (predicateBase: String, aspectOriginal: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) => {
      vecChooser.chooseFrame(predicateBase, aspectOriginal, words, extendedArgs, rand, thr, minFrameCount, useFrameSwapper)
    }
  }

  def srVote(algo: String, useFrameSwapper: Boolean, thr: Double, useExtendedArgs: Boolean) = {
    val wordnet = manager.computeResource[PLWordnet]

    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]
    //val frameSwapperBuilder = new FrameSwapperBuilder(walenty, index, baseCounts.predicateCounts)

    val classifiers: Seq[ScoreBasedVerbClassifier] = algo.split(";").map {
      case "clark" => manager.computeResource[ClarkWeirVerbClassifier]
      case "splitsum" => manager.computeResource[SplitSumVerbClassifier]
      case "selassoc" => manager.computeResource[SelectionalAssociationClassifier]
      case "treecut" => ???
    }.toSeq

    println(s"SrVote algo=$algo, useFrameSwapper=$useFrameSwapper, thr=$thr, useExtendedArgs=$useExtendedArgs")

    val srVoteFrameChooser = new SRVoteFrameChooser(index, walenty, classifiers, frameSwapperBuilder, baseCounts)

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
      if (useExtendedArgs)
        srVoteFrameChooser.chooseFrame(predicateBase, aspect, words, extendedArgs, useFrameSwapper, thr)
      else
        srVoteFrameChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments, useFrameSwapper, thr)

  }

  def srPerc(algo: String,thr: Double) /* TODO: other params */ = {
    val wordnet = manager.computeResource[PLWordnet]

    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]

    val classifier = manager.computeResource[SplitSumVerbClassifier]
    val scores = manager.computeResource[SplitSumCounts]
    println(s"SrPerc algo=$algo, thr=$thr")



    val maps = percentiles.get()

    val frameSwapperBuilder = new FrameSwapperBuilder(walenty, index, baseCounts.predicateCounts)

    val srVoteFrameChooser = new SRVotePercentileFrameChooser(index, walenty, classifier, maps, frameSwapperBuilder, baseCounts)

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
      srVoteFrameChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments, false, thr)

  }

  def srVoteOne(algo: String, minRange: Int, maxRange: Int, useSwapper: Boolean) /* TODO: other params */ = {
    val wordnet = manager.computeResource[PLWordnet]

    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]

    val classifier = manager.computeResource[SplitSumVerbClassifier]
    val scores = manager.computeResource[SplitSumCounts]
    println(s"SrVoteOne algo=$algo, useFrameSwapper=false, range=($minRange, $maxRange), useSwapper=$useSwapper")

    /*var i = 0
    val total = baseCounts.predicateGroupCounts.size

   def withPar[K, V](x: collection.Map[K, V]) = {
     val z = x.par
     z.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(20))
     z
   }

                // TODO: provider (PhD.jar like one) and caching!
                // TODO: in phd -- caching of SR counts!
    val mapsP = baseCounts.predicateGroupCounts.map { case (k, v) =>
      if ((i % 1000) == 0) println(s"$i / ${total}")
      i += 1
      val synsetMap = scores.counts.forPredicateGroupType(k._1, k._2)
      val important = synsetMap.filter(_._2 > 0.2).map(_._1)
      val sorted = important.map {
        s =>
          val hypernyms = wordnet.hypernymyPathsForSynsets(s).view.flatMap(_.toSet).toSet
          val hscored1: (Int, Float) = hypernyms.map { sh =>
            sh -> synsetMap.getOrElse(sh, 0.0f) / v
          }.maxBy(_._2)
          hscored1._2
      }.toSet.toSeq.sorted

      if (sorted.nonEmpty)
        k -> (sorted(sorted.size * minRange / 100) -> sorted(sorted.size * maxRange / 100))
      else
        k -> (0.0f, 0.0f)
    }*/

    val maps = boundaries.get(minRange, maxRange)

    //val frameSwapperBuilder = new FrameSwapperBuilder(walenty, index, baseCounts.predicateCounts)

    val srVoteFrameChooser = new SRVoteOneClassFrameChooser(index, walenty, classifier, maps, frameSwapperBuilder, baseCounts)

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
        srVoteFrameChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments, useSwapper, 0.0)

  }

  // TODO: hopefully this still will work for spark...
  lazy val nbClassifier = new NaiveBayesArgumentsClassifier(manager.computeResource[PredicateGroupDataset], index, baseCounts.directScores)
  def bayes(useFrameSwapper: Boolean, thr: Double, thrRatio: Double, useExtendedArgs: Boolean, onlyMaxFrames: Boolean) = {
    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]
    val dataset = manager.computeResource[PredicateGroupDataset]

    println(s"Bayes useFrameSwapper=$useFrameSwapper, thr=$thr, thrRatio=$thrRatio, useExtendedArgs=$useExtendedArgs")
    // val nbClassifier = new NaiveBayesArgumentsClassifier(manager.computeResource[PredicateGroupDataset], index, baseCounts.directScores)
    val bayesChooser = new NaiveBayesFrameChooser(walenty, nbClassifier, frameSwapperBuilder)

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
      if (useExtendedArgs)
        bayesChooser.chooseFrame(predicateBase, aspect, words, extendedArgs, useFrameSwapper, thr, thrRatio, onlyMaxFrames)
      else
        bayesChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments, useFrameSwapper, thr, thrRatio, onlyMaxFrames)
  }

  def negbayes(useFrameSwapper: Boolean, thr: Double, thrRatio: Double, useExtendedArgs: Boolean) = {
    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]
    val dataset = manager.computeResource[PredicateGroupDataset]

    println(s"NegBayes useFrameSwapper=$useFrameSwapper, thr=$thr, thrRatio=$thrRatio, useExtendedArgs=$useExtendedArgs")

    val bayesChooser = new NegativeNaiveBayesFrameChooser(walenty, nbClassifier)

    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
      if (useExtendedArgs)
        bayesChooser.chooseFrame(predicateBase, aspect, words, extendedArgs, useFrameSwapper, thr, thrRatio)
      else
        bayesChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments, useFrameSwapper, thr, thrRatio)
  }


}