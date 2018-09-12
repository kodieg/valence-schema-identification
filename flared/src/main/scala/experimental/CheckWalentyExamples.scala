package experimental

import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty._
import main.pas.EvaluateFrameMatchingWith
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Created by kodie on 3/3/16.
  * TODO: this is debug only
 */
object CheckWalentyExamples extends App {
  val config = new SparkConf().setMaster("local[*]").setAppName("test-app").set("spark.executor.memory", "40g")
  implicit val sc = new SparkContext(config)


  //val inputPath = args.lift(1).getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-c-0.5-minFreq-1-label-P-with-lexsiebiecp/walenty-examples-keep-labels-2-keep-thr-0.9-usecp-true.obj")
  val inputPath = args.lift(1).getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-L1-c-0.5-minFreq-1-label-I/walenty-examples-keep-labels-2-keep-thr-0.9-usecp-true.obj")
  val outputPath = args.lift(2)
  val dataRaw = sc.objectFile[((Sentence, Seq[ArrayBuffer[ArgumentWithProb]]), Map[String, String])](inputPath)

  val walenty = NewWalentyParser()
  val maxFrameChooser = new MaximalFrameChooser(walenty, TrueFilterer)
  val bcFrameChooser = sc.broadcast(maxFrameChooser)

  /*val easyCases = sc.accumulator(0)
  val totalNotNone = sc.accumulator(0)*/
  //val predicateErrors = sc.accumulator(0)

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



  val results = dataWithPredicates.flatMap {
    case ((sentence, args), walentyAnn) =>
      if (sentence.words.exists(_.orth == "groszy")) {
        Some((sentence, args, walentyAnn))
      } else {
        None
      }
  }

  //val ex = results.filter(_._3.getOrElse("wybor argumentow", "").contains("jako,")).take(100)
  val ex = results.filter(x => x._1.words.exists(_.orth == "parę") && x._1.words.exists(_.orth == "groszy")).cache()

  println(ex.collect())



  // To działa na pozycjach syntaktycznych
  def parseGoldArgs(args: String) = {
    def parseSingleArg(arg: String) = {
      val start = arg.indexOf('[') + 1
      val end = arg.indexOf("} <") + 1
      //println(s"IN $arg --> ${arg.substring(start, end).trim()}")

      arg.substring(start, end).trim()
    }
    BracketAwareSplitter('+', args).map(parseSingleArg)
  }

  // Ppoprzednia metoda liczy pozycje syntaktyczne
  def parseGoldArgsReal(args: String) = {
    def parseSingleArg(arg: String) = {
      val end = arg.lastIndexOf('>')
      val start = arg.indexOf("} <") + 3
      //println(s"IN $arg --> ${arg.substring(start, end).trim()}")

      BracketAwareSplitter(';', arg.substring(start, end).trim())
    }
    BracketAwareSplitter('+', args).map(parseSingleArg)
  }

  ex.foreach { case (sentence, argsRaw, walentyAnn) =>
    val goldPredicate = walentyAnn("haslo")
    val goldFrame = walentyAnn("ramka")
    val goldArgs = parseGoldArgs(walentyAnn.getOrElse("wybor argumentow", "")).toIndexedSeq.toSet
    val goldArgsReal = parseGoldArgsReal(walentyAnn.getOrElse("wybor argumentow", "")).toIndexedSeq.toSet

    // Filtered out cases where cannot find predicate.
    val predicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2).get
    val predicateIndexes = argsRaw.flatten.map(_.predicate.left).toSet.toSeq.sorted
    val leftNeighbour = Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
    val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption

    val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"
    val hasSieArgument = goldFrame.startsWith("się")
    val possibleFrames = walenty.framesFor(goldPredicate, aspect, hasSieArgument).toIndexedSeq

    val skladnicaArgumentsWithoutSie = argsRaw.map(_.filter(_.argumentType != "sie"))

    //println(skladnicaArgumentsWithoutSie.map(_.size), skladnicaSingleArgs.map(_.size))
    val single = skladnicaArgumentsWithoutSie.map(x => if (x.size > 1) x.take(0) else x)

    // z jakiegoś powodu jak dam take(1) to nie daje takich samych wynikow jak uzycie pliku bez -multi!
    // chyba chodzi o to, że skipowany jest '_'?
    val usedSkladnicaArgs = /* single // */ skladnicaArgumentsWithoutSie


    val correctFrame = possibleFrames.find(frame => EvaluateFrameMatchingWith.comparableFormat(frame.frameText) == EvaluateFrameMatchingWith.comparableFormat(goldFrame))
    if (correctFrame.isEmpty) {
      println(s"Could not find frame for $goldFrame from $possibleFrames for $goldPredicate / $aspect / $hasSieArgument")
    }


    val wa = correctFrame.map { frame =>
      val argumentsMatching = WalentyArgumentMatcher.matchingElements(sentence.words, usedSkladnicaArgs, frame)
      val waletnyMatchedArgs = argumentsMatching.map(_.map(_._2.text).getOrElse(""))
      waletnyMatchedArgs
    }.getOrElse(argsRaw.map(_ => ""))

    val args = argsRaw.map(_.headOption.map(_.argumentType).getOrElse("_"))
    val wordLengths = (sentence.words, args, wa).zipped.map {
      case (a, b, c) => List(a.orth.size, a.ctag.size, b.size, c.size, 2).max
    }
    val text = sentence.words.zip(wordLengths).map(aw => aw._1.orth.padTo(aw._2, ' ')).mkString(" ")
    val ctagtext = sentence.words.zip(wordLengths).map(aw => aw._1.ctag.padTo(aw._2, ' ')).mkString(" ")
    val labArgs = args.zip(wordLengths).map(aw => aw._1.padTo(aw._2, ' ')).mkString(" ")

    val word = sentence.words.find(_.orth == "jako")

    val waStr = wa.zip(wordLengths).map(wl => wl._1.padTo(wl._2, ' ')).mkString(" ")
    val matched = BaseArgumentsMatcher.matches("prepnp(jako,str)", WalentyArgument("arg", Seq.empty, Seq("prepnp(jako,str)")), "")

    //if (walentyAnn.getOrElse("wybor argumentow", "").contains("prepnp(jako,") && !waStr.contains("prepnp(jako,")) {
      println(text)
      println(ctagtext)
      println(labArgs)
      println(waStr)
      println(walentyAnn.getOrElse("wybor argumentow", ""))
      println(word -> matched)
      println()
    //}


  }
}

