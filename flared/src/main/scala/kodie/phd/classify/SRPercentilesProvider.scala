package kodie.phd.classify

import java.io._
import java.util.concurrent.atomic.AtomicReference

import kodie.core.soa.ServiceManager
import phd.sr.counts.BaseCounts
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.SplitSumCounts
import phd.sr.scorer.pd.SplitSumVerbClassifier

import scala.util.control.NonFatal

/**
 * Created by kodie on 9/7/16.
 */
class SRPercentilesProvider(manager: ServiceManager, cacheBasePath: String) {

  val instance = new AtomicReference[collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), Array[Float]]](null)

  def get() = {
    if (instance.get() == null) {
      instance.synchronized {
        if (instance.get() == null) {
          instance.set(_get())
        }
      }
    }

    instance.get()
  }

  def _get() : collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), Array[Float]] = {
    val cachePath = cacheBasePath + s"/srpercentiles.bin"
    if (new File(cachePath).exists) {
      try {
        return loadCache(cachePath)
      } catch {
        case NonFatal(e) => println(s"Exception: $e")
      }
    }

    return compute(cachePath)
  }

  def loadCache(path: String) = {
    val ios = new ObjectInputStream(new FileInputStream(path))
    val counts = ios.readObject().asInstanceOf[collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), Array[Float]]]
    ios.close()
    counts
  }

  def saveCache(path: String, counts: collection.Map[(PredicateGroupDataset.PredicateLemma, PredicateGroupDataset.GroupType), Array[Float]]) {
        val oos = new ObjectOutputStream(new FileOutputStream(path))
        oos.writeObject(counts)
        oos.close()
  }

  private[this] def compute(path: String) = {
    val wordnet = manager.computeResource[PLWordnet]

    val baseCounts = manager.computeResource[BaseCounts]
    val index = manager.computeResource[WordsIndex]

    val classifier = manager.computeResource[SplitSumVerbClassifier]
    val scores = manager.computeResource[SplitSumCounts]

    var i = 0
      val total = baseCounts.predicateGroupCounts.size

    // todo... wouldn't it be better to calculate distribution over examples from coropus
    // and not over synsets that are considered important?!
    // then probably problem will be with -1 classification
    //  (But here is it really better with important synsets only?)
    val mapsP = baseCounts.predicateGroupCounts.map { case (k, v) =>
      if ((i % 1000) == 0) println(s"$i / ${total}")
      i += 1
      val synsetMap = scores.counts.forPredicateGroupType(k._1, k._2)
      // important causes that only positive/very positive examples are taken into account!
      // how bad it is? we need positive examples...
      val important = synsetMap.filter(_._2 > 0.2).map(_._1)
      val sorted = important.map {
        s =>
          val hypernyms = wordnet.hypernymyPathsForSynsets(s).view.flatMap(_.toSet).toSet
          val hscored1: (Int, Float) = hypernyms.map { sh =>
            sh -> synsetMap.getOrElse(sh, 0.0f) / v
            //??? is this score / v correct? --- maybe it's not?
          }.maxBy(_._2)
          hscored1._2
      }.toSet.toSeq.sorted

      val ssize = sorted.size
      val values = if (ssize > 0) {
        Array.tabulate(100) { i =>
          sorted(ssize * i / 100)
        }
      } else {Array(0.0f)}
      k -> values
    }

    val maps = mapsP
    saveCache(path, maps)
    mapsP
  }
}
