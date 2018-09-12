package main.pas

import java.io.PrintWriter

import experimental.{Argument, ArgumentWithProb}
import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty.{WalentyArgumentMatcher, TrueFilterer, MaximalFrameChooser, NewWalentyParser}
import org.apache.spark.{SparkContext, SparkConf}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Created by kodie on 1/28/16.
 */
object EvaluateMatchingHeuristic extends App {
  val config = new SparkConf().setMaster("local[*]").setAppName("test-app").set("spark.executor.memory", "40g")
  implicit val sc = new SparkContext(config)


  // Wersja z pojedynczymi typami argumentów!
  /*val inputPath = args.lift(1).getOrElse("data/pantera-new-walentyExamples-with-args-and-ann.obj")
  val dataRaw = sc.objectFile[((Sentence, Seq[Option[Argument]]), Map[String, String])](inputPath).map { case ((a, b), c) =>
    ((a, b.map(_.to[ArrayBuffer].map(arg => ArgumentWithProb(arg.argumentType, arg.predicate, arg.semanticHead, 1.0)))), c)
  }*/

  // Wersja z multi-class (do 3 typów)!
  val inputPath = args.lift(0).getOrElse("data/pantera-new-multi-with-cp-heur-walentyExamples-with-args-and-ann.obj")
  val outputPath = args.lift(1)
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
      isVerbFrame(goldFrame) && !goldFrame.contains("{or}")
  }.cache()


  val dataWithPredicates = data.filter {
    case ((sentence, args), walentyAnn) =>

      val goldPredicate = walentyAnn("haslo")
      val goldFrame = walentyAnn("ramka")

      val maybePredicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
      sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2)
      maybePredicateIndex != None
  }.cache()


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

  val results = dataWithPredicates.flatMap {
    case input@((sentence, args), walentyAnn) =>
      val goldPredicate = walentyAnn("haslo")
      val goldFrame = walentyAnn("ramka")
      val goldArgs = parseGoldArgs(walentyAnn.getOrElse("wybor argumentow", "")).toIndexedSeq.toSet
      val goldArgsReal = parseGoldArgsReal(walentyAnn.getOrElse("wybor argumentow", "")).toIndexedSeq.toSet

      // Filtered out cases where cannot find predicate.
      val predicateIndex = sentence.words.zipWithIndex.find(_._1.base == goldPredicate).map(_._2).get
      val predicateIndexes = args.flatten.map(_.predicate.left).toSet.toSeq.sorted
      val leftNeighbour = Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
      val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption

      val aspect = if (goldFrame.contains(":imperf:")) "imperf" else if (goldFrame.contains(":perf:")) "perf" else "_"
      val hasSieArgument = goldFrame.startsWith("się")
      val possibleFrames = walenty.framesFor(goldPredicate, aspect, hasSieArgument).toIndexedSeq

      val skladnicaArgumentsWithoutSie = args.map(_.filter(_.argumentType != "sie"))

      //println(skladnicaArgumentsWithoutSie.map(_.size), skladnicaSingleArgs.map(_.size))
      val single = skladnicaArgumentsWithoutSie.map(x => if (x.size > 1) x.take(0) else x)

      // z jakiegoś powodu jak dam take(1) to nie daje takich samych wynikow jak uzycie pliku bez -multi!
      // chyba chodzi o to, że skipowany jest '_'?
      val usedSkladnicaArgs = /* single // */ skladnicaArgumentsWithoutSie


      val correctFrame = possibleFrames.find(frame => EvaluateFrameMatchingWith.comparableFormat(frame.frameText) == EvaluateFrameMatchingWith.comparableFormat(goldFrame))
      if (correctFrame.isEmpty) {
        println(s"Could not find frame for $goldFrame from $possibleFrames for $goldPredicate / $aspect / $hasSieArgument")
      }

      // Nie rozumiem dlaczego skladnicaSingleArgs i multi args daje takie same wyniki :|
      correctFrame.map { frame =>
        val argumentsMatching = WalentyArgumentMatcher.matchingElements(sentence.words, usedSkladnicaArgs, frame)
        val goldArgsWithoutRefl = goldArgs.filter(_ != "{refl}")
        val waletnyMatchedArgs = argumentsMatching.flatten.map(_._2.text).toSet.filter(_ != "{refl}")
        // TODO: nie bardzo umiem sprawdzic ktora z realizacji została dopasowana! a to byloby potrzebne zeby robic
        // ewaluację na poziomie argumentów a nie pozycji!
        val corrects = waletnyMatchedArgs.intersect(goldArgsWithoutRefl).size
        val subtotalForRecall = goldArgsWithoutRefl.size
        val subtotalForPrecision = waletnyMatchedArgs.size
        //println(s"GOT $waletnyMatchedArgs and gold is: $goldArgs -- ${goldArgs.intersect(waletnyMatchedArgs)}")
        (corrects, subtotalForRecall, subtotalForPrecision, goldArgsWithoutRefl, waletnyMatchedArgs)
      }

  }

  val res = results.collect
  val accuracy = (res.filter(x => x._1 == x._2).size.toFloat) / res.size
  val correct = res.map(_._1).sum.toFloat
  val totalForRecall = res.map(_._2).sum.toFloat
  val totalForPrecision = res.map(_._3).sum.toFloat

  val mapCorrect   = mutable.Map[String, Int]().withDefaultValue(0)
  val mapTotalPrec = mutable.Map[String, Int]().withDefaultValue(0)
  val mapTotalRec  = mutable.Map[String, Int]().withDefaultValue(0)
  val totalErrors = totalForRecall - correct

  for (r <- res) {
    val correct = r._4.intersect(r._5)
    correct.foreach { c => mapCorrect(c) += 1 }
    r._4.foreach { c => mapTotalRec(c) += 1 }
    r._5.foreach { c => mapTotalPrec(c) += 1 }
  }

  def divz(f: Int, d: Int) = if (d == 0) Float.NaN else f.toFloat / d.toFloat

  val out = outputPath.map(new PrintWriter(_))

  out.foreach(_.println(s"Sentence level Accuracy: ${accuracy}"))
  out.foreach(_.println(s"Total results: ${correct} corrects, prec: ${correct/totalForPrecision} recall: ${correct/totalForRecall}"))

  println(s"Sentence level Accuracy: ${accuracy}")
  println(s"Total results: ${correct} corrects, prec: ${correct/totalForPrecision} recall: ${correct/totalForRecall}")
  val keys = mapTotalRec.keys.toSet ++ mapTotalPrec.keys.toSet
  keys.toSeq.filter(k => mapTotalRec(k) > 10).sortBy {
    k => //-mapCorrect(k)
      val errors = (mapTotalRec(k) - mapCorrect(k)).toFloat
      -errors
  } foreach { k =>
    val errors = (mapTotalRec(k) - mapCorrect(k)).toFloat
    out.foreach(_.println(s"For key: $k correct = ${mapCorrect(k)}, prec (${mapTotalPrec(k)}): ${divz(mapCorrect(k),mapTotalPrec(k))}, recall (${mapTotalRec(k)}): ${divz(mapCorrect(k),mapTotalRec(k))}, %err: ${errors/totalErrors}"))
    println(s"For key: $k correct = ${mapCorrect(k)}, prec (${mapTotalPrec(k)}): ${divz(mapCorrect(k),mapTotalPrec(k))}, recall (${mapTotalRec(k)}): ${divz(mapCorrect(k),mapTotalRec(k))}, %err: ${errors/totalErrors}")
  }

  out.foreach(_.flush())
  out.foreach(_.close())
}
