package main.pas

import java.io.PrintWriter

import experimental.ArgumentWithProb
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.skladnica.types.{Sentence, Word}
import kodie.phd.walenty._
import main.pas.EvaluateFrameMatchingWith._
import _root_.main.pas.ExtractCorpusForSRWith.outputPath
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import phd.sr.data.PredicateGroupDataset
import phd.sr.external.plwn.PLWordnet
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


/**
 * Created by kodie on 8/11/15.
 */
object ExtractCorpusForSRWith extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }

  val (newArgs1, outPath) = if (newArgs0(0) == "--output") {
    (newArgs0.drop(2), Some(newArgs0(1)))
  } else {
    (newArgs0, None)
  }

  val (newArgs2, algosStr) = if (newArgs1(0) == "--algo") {
    (newArgs1.drop(2), newArgs1(1))
  } else {
    (newArgs1, "max")
  }

  val (newArgs, formatStr) = if (newArgs2(0) == "--format") {
    (newArgs2.drop(2), newArgs2(1))
  } else {
    (newArgs2, "sr-text")
  }

  if (!Seq("sr-text", "conll").contains(formatStr)) {
    println("Invalid format. Supported only binary and conll")
    System.exit(1)
  }

  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }

  val wordnet = manager.computeResource[PLWordnet]
  val chooserCascade = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent).fromString(algosStr)

  val config = new SparkConf().setMaster("local[20]").setAppName("test-app").set("spark.executor.memory", "40g").set("spark.driver.maxResultSize", "10g")
  implicit val sc = new SparkContext(config)


  val inputPath = path.getOrElse("_/home/kodie/Flared/data/pantera-nkjp-300m-sentence-CRF-c-0.5-keep-2-thr-0.9.obj")
  val outputPath = outPath.getOrElse("data/nkjp300m-max-frame-sr-custom-corpus-c-0.5-keep-2-thr-0.9-P.txt")

  val data = sc.objectFile[(Sentence, Seq[ArrayBuffer[ArgumentWithProb]])](inputPath)

  val counter = sc.accumulator(0, "counter")
  val examples = sc.accumulator(0, "examples")

  val dataWithPredicates = data.flatMap {
    case (sentence, args) =>
      val predicateIndexes = args.flatten.map(_.predicate.left).toSet
      val cases = predicateIndexes.map { predicateIndex =>
        val predicateWord = sentence.words(predicateIndex)
        val arguments = args.map(_.filter(_.predicate.left == predicateIndex))
        (sentence, predicateWord, predicateIndex, arguments)
      }

      cases
  }



  val results = dataWithPredicates.map {
    case input@(sentence, predicate, predicateIndex, predicateArgs) =>
      examples += 1
      val base = predicate.base
      val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"

      val withProbs = predicateArgs

      val predicateIndexes = predicateArgs.flatten.map(_.predicate.left).toSet.toSeq.sorted
      val leftNeighbour =  Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
      val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption

      val leftNeighIndex = leftNeighbour.getOrElse(0)
      val rightNeighIndex = rightNeighbour.getOrElse(sentence.words.size)

      val extendedArgs = predicateArgs.zipWithIndex.map(pair => pair._1.filter { arg =>
        arg.predicate.left == predicateIndex || (pair._2 >= leftNeighIndex && pair._2 <= rightNeighIndex) } )

      val output @ (predicted, debugInfo, _) = chooserCascade.chooseFrame(base, aspect, sentence.words, withProbs, extendedArgs)
      (input, output)
  }

  def saveAndCat(outputPath: String, data: RDD[String]): Unit = {
    data.saveAsTextFile(outputPath + "-tmp")

    {
      import sys.process._
      (s"find ${outputPath}-tmp -name part*" #| "xargs cat" #> new java.io.File(s"${outputPath}")).!
      s"rm -rf ${outputPath}-tmp".!
      //println(s"You need to `cat data/tmp-nkjp300-sr-corpus-in-parts.txt > data/nkjp300m-max-frame-sr-custom-corpus-c-0.5-keep-2-thr-0.9-P.txt`")
    }

  }

  if (formatStr.equalsIgnoreCase("sr-text")) {

    val resultCorpus = results.flatMap {
      case (input, output) if output._1.isDefined =>
        counter += 1
        val sentence = input._1
        val predicate = input._2
        val arguments = input._3
        val frame = output._1.get

        val lines = frame.argumentsMatching /*arguments.zip(frame.matchedArguments)*/ .collect {
          case Some((argument, matched)) if argument.semanticHead.isDefined =>
            val semHeadIndex = argument.semanticHead.get
            val semHeadWord = sentence.words(semHeadIndex)
            val groupType = matched.text
            val predicateWithFrame = frame.frame.frameText

            val lineParts = Seq(predicateWithFrame, predicate.orth, semHeadWord.base, semHeadWord.orth, groupType)
            val line = lineParts.map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
            line
        }
        lines
      case _ => Seq.empty
    }

    println(counter.value, examples.value)
    saveAndCat(outputPath, resultCorpus)
  } else {
    val resultCorpus = results.flatMap {
      case ((sentence, predicate, predicateIndex, predicateArgs), (predicted, debugInfo, x)) if predicted.isDefined =>

        Seq.tabulate(sentence.words.size) { i =>
          val word = sentence.words(i)
          val isPredicate = if (i == predicateIndex) { "predicate" } else "_"
          val predValue   = if (i == predicateIndex) { predicted.get.frame.frameText } else { "_" }
          val walentyArgument = predicted.get.argumentsMatching.lift(i).getOrElse(None).map(_._2.text).getOrElse("_")
          s"${word.orth}\t${word.base}\t${word.ctag}\t${predicateArgs(i).lift(0).map(_.argumentType).getOrElse("_")}\t${walentyArgument}\t${isPredicate}\t$predValue"
        } ++ Seq("\n")
      case _ => Seq.empty
    }

    saveAndCat(outputPath, resultCorpus)
  }

}
