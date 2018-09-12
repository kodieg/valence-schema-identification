package kodie.phd.classify

import java.io._
import java.util.concurrent.atomic.AtomicReference

import kodie.core.soa.ServiceManager
import phd.sr.counts.BaseCounts
import phd.sr.data.PredicateGroupDataset.{GroupType, PredicateLemma}
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.SplitSumCounts
import phd.sr.scorer.pd.{VerbPseudoDisambiguationInstance, SplitSumVerbClassifier}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.util.control.NonFatal
import scala.collection.mutable

/**
 * Created by kodie on 9/7/16.
 */
class SRBoundariesProvider(manager: ServiceManager, cacheBasePath: String) {

  def get(minRange: Int, maxRange: Int) : collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), (Float, Float)] = {
    val cachePath = cacheBasePath + s"/srboundaries-$minRange-$maxRange.bin"
    if (new File(cachePath).exists) {
      try {
        return loadCache(cachePath)
      } catch {
        case NonFatal(e) => println(s"Exception: $e")
      }
    }

    return compute(minRange, maxRange, cachePath)
  }

  def loadCache(path: String) = {
    val ios = new ObjectInputStream(new FileInputStream(path))
    val counts = ios.readObject().asInstanceOf[collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), (Float, Float)]]
    ios.close()
    counts
  }

  def saveCache(path: String, counts: collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), (Float, Float)]) {
        val oos = new ObjectOutputStream(new FileOutputStream(path))
        oos.writeObject(counts)
        oos.close()
  }

  private[this] def compute(minRange: Int, maxRange: Int, path: String) = {
    val wordnet = manager.computeResource[PLWordnet]

    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]
    val dataset = manager.computeResource[PredicateGroupDataset]

    val classifier = manager.computeResource[SplitSumVerbClassifier]
    val scores = manager.computeResource[SplitSumCounts]

    var i = 0

    val scoresSeq = mutable.Map[(PredicateLemma, GroupType), mutable.ArrayBuffer[Float]]()
/*
    dataset.foreach { instance =>
      if ((i % 10000) == 0) println(s"$i")
      i += 1
      val example = VerbPseudoDisambiguationInstance(0, instance.predicateLemma, instance.predicateLemma, instance.groupSemanticHeadLemma, instance.groupSemanticHeadOrth, instance.groupType, instance.groupType)
      val score: Float = classifier.pickWithDebug(example)._2._1._2.toFloat
      if (score > 0) {
        scoresSeq.getOrElseUpdate(instance.predicateLemma -> instance.groupType, new mutable.ArrayBuffer[Float]()) += score
      }
    }

    val mapsP = scoresSeq.map { case (k, v) =>
      val sorted = v.sorted
      val size = sorted.size

        if (size == 0) {
          k -> (0.0f, 0.0f)
        } else {
          k -> (sorted(size * minRange / 100) -> sorted(size * maxRange / 100))
        }
    }
*/

    val total = baseCounts.predicateGroupCounts.size

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
    }

    val maps = mapsP
    saveCache(path, maps)
    mapsP
  }
}
