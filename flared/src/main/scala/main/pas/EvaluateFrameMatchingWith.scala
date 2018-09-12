package main.pas

import java.io.{File, FileOutputStream, PrintWriter}

import experimental.ArgumentWithProb
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty._
import org.apache.commons.io.FileUtils
import org.apache.spark.{SparkConf, SparkContext}
import phd.sr.data.PredicateGroupDataset
import phd.sr.external.plwn.PLWordnet
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


object EvaluateFrameMatchingWith extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }

  val (newArgs, algosStr) = if (newArgs0(0) == "--algo") {
    (newArgs0.drop(2), newArgs0(1))
  } else {
    (newArgs0, "max")
  }

  val config = new SparkConf().setMaster("local[40]").setAppName("test-app").set("spark.executor.memory", "30g")
  implicit val sc = new SparkContext(config)

  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }

  val wordnet = manager.computeResource[PLWordnet]
  val chooser = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent).fromString(algosStr)

  val inputPath = path.getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-L1-c-1.0-minFreq-1-label-P/walenty-examples-keep-labels-2-keep-thr-0.9-usecp-true.obj")
  val allData = sc.objectFile[((Sentence, Seq[ArrayBuffer[ArgumentWithProb]]), Map[String, String])](inputPath)

  println(s"Input path: $inputPath")

  val walenty = NewWalentyParser()

  val dataRaw = allData.filter { case ((sentence, args), walentyAnn) =>
    val pos = WalentyExamplesUtils.argsToWalentyPositions(walentyAnn.getOrElse("wybor argumentow", ""))
    val goldPredicate = walentyAnn("haslo")
    val goldFrame = walentyAnn("ramka")
    val hasSieArgument = goldFrame.startsWith("się")
    val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"

    val frames = walenty.framesFor(goldPredicate, aspect, hasSieArgument)

    val matchingFrames = frames.filter(f => WalentyExamplesUtils.frameContainsAllPositions(f, pos))
    val isAmbiguous = matchingFrames.size > 1
    if (isAmbiguous) {
      println(s"Removing sentence: ${sentence.text} -- ${matchingFrames.map(_.frameText).mkString("@")}")
    }
    val newCorpusAmbig = walentyAnn.get("supercomment").orElse(walentyAnn.get("comment1").filter(_.toLowerCase.contains("niejedn")).orElse(walentyAnn.get("comment2"))).exists(_.toLowerCase.contains("niejedn"))
    /*!isAmbiguous &&*/ !newCorpusAmbig && !goldFrame.contains("{or}")
  }

  def isVerbFrame(frame: String) = {
    !frame.startsWith("::::") && !frame.startsWith("::pred::")
  }


  val data = dataRaw.filter {
    case ((sentence, args), walentyAnn) =>
      val goldFrame = walentyAnn("ramka")
      isVerbFrame(goldFrame)
  }.cache()



  val dataWithPredicates = data.filter {
    case ((sentence, args), walentyAnn) =>

    val goldPredicate = walentyAnn("haslo")
    val goldFrame = walentyAnn("ramka")

    val maybePredicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
    sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
    maybePredicateIndex != None
   }.cache()



  val results = dataWithPredicates.map {
    case input@((sentence, args), walentyAnn) =>
      val goldPredicate = walentyAnn("haslo")
      val goldFrame = walentyAnn("ramka")

      // Filtered out cases where cannot find predicate.
      val predicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2).get

      val predicateIndexes = args.flatten.map(_.predicate.left).toSet.toSeq.sorted
      val leftNeighbour =  Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
      val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption

      val leftNeighIndex = leftNeighbour.getOrElse(0)
      val rightNeighIndex = rightNeighbour.getOrElse(sentence.words.size)

      val extendedArgs = args.zipWithIndex.map(pair => pair._1.filter { arg =>
        arg.predicate.left == predicateIndex || (pair._2 >= leftNeighIndex && pair._2 <= rightNeighIndex) } )

      val predicateArgs = args.map(_.filter(p => p.predicate.left == predicateIndex || p.predicate.left == -1))
      val predicate = sentence.words(predicateIndex)
      val base = predicate.base
      val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"

      val outputTriple@(predicted, debugInfo, classifierIndex) = chooser.chooseFrame(base, aspect, sentence.words, predicateArgs, extendedArgs)

      val output = ((predicted, debugInfo), classifierIndex)

      val result = predicted match {
        case None => false
        case Some(chooserResult) => comparableFormat(chooserResult.frame.frameText) == comparableFormat(goldFrame)
      }

      (result, ((sentence, predicateArgs), walentyAnn), output)
  }.cache()

  FileUtils.deleteDirectory(new File("results.bin"))

  results.saveAsObjectFile("results.bin")

  val correct = results.filter(_._1).count
  val totalNotNone = results.filter(_._3._1._1.isDefined).count()
  val easyCases = results.filter(_._3._1._1.map(_.candidatesCount == 1).getOrElse(false)).count()
  val total = data.count()

  val totalByClsId = results.filter(_._3._1._1.isDefined).map(_._3._2 ).countByValue()
  val correctByClsId = results.filter(_._1).map(_._3._2).countByValue()
  val randFramesByClsId = results.filter(_._3._1._1.isDefined).map { x=>
    // for rand calculations -- just include those values that are not swapped!
    val maxFrames = x._3._1._2.events.filter(c => c.frameSwapper == c.frame.frameText)

    val correctInMax = maxFrames.exists(e => comparableFormat(e.frameString) == comparableFormat(x._2._2.getOrElse("ramka",  "???")))
    val avgr = if (correctInMax) {
      1.0 / maxFrames.size
    } else {
      0.0
    }
    val clsId =  x._3._2
    clsId -> avgr
  }.reduceByKey(_ + _).collect().toMap

  val predicateErrors = total - dataWithPredicates.count()

  val algosList = algosStr.split(",")

  val reportNew = new PrintWriter(new FileOutputStream("report.csv"))
  var i = 0
  results.filter(_._3._1._1.isDefined).collect().foreach {
    case (correct, input, (output, clsId)) =>
      val l = algosList.lift(clsId).getOrElse("unknown classifier").contains("max")
      val goldFrame = input._2("ramka")
      val haslo = input._2("haslo")
      val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"


      output._2.events.foreach { ev =>
        val resp = List(i.toString, correct.toString, (comparableFormat(input._2.getOrElse("ramka", "")) == comparableFormat(ev.frame.frameText)).toString, (comparableFormat(output._1.get.frame.frameText) == comparableFormat(ev.frameString)).toString, algosList.lift(clsId).getOrElse("unknown classifier"), clsId.toString, input._1._1.text, haslo, aspect, ev.frameString, ev.frameSwapper) ++ ev.detailerDebug.drop(3)
        reportNew.println(s"${resp.map(_.replace('$', ' ')).mkString("$")}")
      }
      i+=1

  }
  results.filter(_._3._1._1.isEmpty).collect().foreach {
    case (correct, input, (output, clsId)) =>
      val l = algosList.lift(clsId).getOrElse("unknown classifier").contains("max")
      val goldFrame = input._2("ramka")
      val haslo = input._2("haslo")
      val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"


        val resp = List(i.toString, "false", "false", "false", "not chosen", "-1", input._1._1.text, haslo, aspect)
        reportNew.println(s"${resp.map(_.replace('$', ' ')).mkString("$")}")
      i+=1

  }
  reportNew.close()

  val report = new PrintWriter(new FileOutputStream("errors.txt"))

  results.filter(res => res._3._1._1.isEmpty).collect().foreach {
    case (_, input, (output, clsId)) =>
      val ((sentence, args), walentyAnn) = input
      val predictedFrame = output._1
      val debugInfo = output._2

      report.println("-------------------------------------------------------------------------------------------")
      report.println(s"Expected: ${walentyAnn("ramka")}")
      report.println(s"Got: ${predictedFrame.map(_.frame.frameText).getOrElse("none")}\n")
      report.println(s"Sentence: ${sentence.words.map(_.orth).mkString(" ")}")
      report.println(s"Sentence arguments: ${sentence.argumentStructures}")
      report.println(s"Sentence fl: ${sentence.freePhrases}\n")
      report.println(s"Arguments: ${args.flatten.map(arg => s"${arg.argumentType}:${arg.semanticHead.map(idx => sentence.words(idx).base)}").mkString(", ")}")
      report.println(s"Args count: ${args.flatten.size}\n")
      report.println(s"Algorithm logs ($clsId, candidates ${predictedFrame.map(_.candidatesCount).getOrElse(-1)} == events: ${debugInfo.events.size}):")

      var matchedSum = 0
      debugInfo.events.foreach { event =>
        val matchedRepr = event.matchedElements.collect {
          case (Some(arg), Some(walenty)) =>
            matchedSum += 1
            s"( ${arg.argumentType} -> ${walenty.modifiers.mkString(",")}{${walenty.realizations.mkString(" + ")}} )"
        }.mkString(", ")

        report.println(s"\t${event.frameString}   ${event.info}    ${matchedRepr}")
      }
      val hasCorrectInEvents = debugInfo.events.exists(event => comparableFormat(event.frameString) == comparableFormat(walentyAnn("ramka")))
      report.println(s"events: ${debugInfo.events.size}, potentially correct: ${if (hasCorrectInEvents) 1 else 0} ")

      report.println(s"Matched args sum: ${matchedSum}")
      report.println()
  }
  report.close()

  println(s"ALGORITHM: $algosStr")
  println(s"Total: $total, errors $predicateErrors, totalNotNone: $totalNotNone, correnct: $correct, easyCases: $easyCases")
  
  println("Overall result: " + correct.toDouble / total)
  println("Matching precision: " + correct.toDouble / totalNotNone)
  println("Ambigous cases matching precision: " + (correct - easyCases).toDouble / (totalNotNone - easyCases))

  println(s"PredicateErrors ${predicateErrors} out of $total")

  for (kv <- totalByClsId.toList.sorted) {
    val rand = randFramesByClsId.getOrElse(kv._1, 0.0) / kv._2
    println(s"ClassfierId ${kv._1} --> ${correctByClsId.getOrElse(kv._1, 0L)} / ${kv._2} == ${correctByClsId.getOrElse(kv._1, 0L).toDouble / kv._2} (random: $rand) -- ${algosList.lift(kv._1).getOrElse("unknown classifier?")}")
  }


  def comparableFormat(x: String) = {
    val f = BracketAwareSplitter(':', x).last.replace(" ", "")
     // ensure that position order within schema is not important!
     BracketAwareSplitter('+',f).sorted.mkString("+")
  }
}
