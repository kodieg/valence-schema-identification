package main.pas

import java.io.{FileOutputStream, PrintWriter}

import experimental.ArgumentWithProb
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.skladnica.types
import kodie.phd.skladnica.types.Sentence
import kodie.phd.utils.NKJP300Freq
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty._
import org.apache.spark.{SparkConf, SparkContext}
import phd.sr.counts.{BaseCounts, VerbsCounts}
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.pd._
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/* Is this percs.txt file used anywhere? */

object DumpPercentiles extends App {



  val globalConfig = Configuration.parse[GlobalConfiguration](args)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }


  val wordnet = manager.computeResource[PLWordnet]
  val chooser = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent)

  val report = new PrintWriter(new FileOutputStream("percs.txt"))

  chooser.percentiles.get().foreach {
    case ((lemmaIdx, groupIdx), percs) =>
      val lemma = chooser.index.getWord(lemmaIdx).getOrElse("?")
      val group = chooser.index.getWord(groupIdx).getOrElse("?")
      for (kv <- percs.zipWithIndex) {
        report.println(List(lemma,group,kv._2.toString,kv._1.toString).mkString("$"))
      }
  }

  report.close()
}
