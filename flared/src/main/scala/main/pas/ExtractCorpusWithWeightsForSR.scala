package main.pas

import java.io.PrintWriter

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty._
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer


/**
 * Created by kodie on 8/11/15.
 */
object ExtractCorpusWithWeightsForSR extends App {
  val config = new SparkConf().setMaster("local[*]").setAppName("test-app").set("spark.executor.memory", "40g").set("spark.driver.maxResultSize", "50g")
  implicit val sc = new SparkContext(config)
  import org.apache.spark.SparkContext._


  //val inputPath = ""
  val inputPath = args.lift(1).getOrElse("data/pantera-nkjp-full-sentence-with-args.obj")
  val outputPath = args.lift(2).getOrElse("data/full-weighted-sr-corpus.txt")

  val data = sc.objectFile[(Sentence, Seq[Option[Argument]])](inputPath).cache()

  val walenty = NewWalentyParser()
  val maxFrameChooser = new MaximalFrameChooser(walenty,
    // TrueFilterer
    new AndFilterer(new MinArgsMatchedFilterer(2), new AndFilterer(InfFilterer, LexicalizedFilterer))
  )
  val multiFrameChooser = new MultiFrameChooser(walenty, new AndFilterer(InfFilterer, LexicalizedFilterer))
  val bcFrameChooser = sc.broadcast(maxFrameChooser)
  val bcMultiFrameChooser = sc.broadcast(multiFrameChooser)

  val counter = sc.accumulator(0, "counter")
  val examples = sc.accumulator(0, "examples")

  /*val easyCases = sc.accumulator(0)
  val totalNotNone = sc.accumulator(0)*/
  //val predicateErrors = sc.accumulator(0)

  val dataWithPredicates = data.flatMap {
    case (sentence, args) =>
      val predicateIndexes = args.flatten.map(_.predicate.left).toSet
      val cases = predicateIndexes.map { predicateIndex =>
        val predicateWord = sentence.words(predicateIndex)
        val arguments = args.map {
          case someArg@Some(arg) if arg.predicate.left == predicateIndex => someArg
          case _ => None
        }
        (sentence, predicateWord, arguments)
      }

      cases
  }



  val results = dataWithPredicates.flatMap {
    case input@(sentence, predicate, predicateArgs) =>
      examples += 1
      val base = predicate.base
      val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"
      val withProbs = predicateArgs.map {
        case (Some(arg)) => ArrayBuffer(ArgumentWithProb(arg.argumentType, arg.predicate, arg.semanticHead, 1.0))
        case None => ArrayBuffer[ArgumentWithProb]()
      }
      val output @ (predicted, debugInfo) = bcFrameChooser.value.chooseMaximalFrame(base, aspect, sentence.words, withProbs)
      if (output._1.isDefined)
        Seq((input, output, 1.0f))
      else {
        val outputs = bcMultiFrameChooser.value.chooseMatchingFrames(base, aspect, sentence.words, withProbs)
        outputs.map(o => (input, o, 1.0f/outputs.size))
      }
  }.cache()

  //val totalNotNone = results.filter(_._2._1.isDefined).count()
  //val easyCases = results.filter(_._2._1.map(_.candidatesCount == 1).getOrElse(false)).count()
  //val total = data.count()

  /*val frameCounts = results.map {
    case (input, output) =>
      val predicateBase = input._2.base
      val selectedFrame = output._1.map(_.frame.frameText)

      (predicateBase, selectedFrame)
  }.countByValue().map {
    case ((predicate, frame), count) =>
      (predicate, (frame, count))
  }.toSeq.groupBy(_._1).mapValues(frames => frames.map(fr => fr._2._1 -> fr._2._2))

  val report = new PrintWriter(new FileOutputStream("frames-counts.txt"))
  // Debugging information
  frameCounts.foreach {
    case (predicate, frameCounts) =>
      report.println("-------------------------------------------------------------------------------------------")
      report.println(s"Predicate: $predicate\n")
      frameCounts.foreach {
        case (frame, count) =>
          report.println(s"\t$frame ==> $count")
      }

      report.println()
  }

  report.close()
  */

  val resultCorpus = results.flatMap {
    case (input, output, caseWeight) if output._1.isDefined =>
      counter += 1
      val sentence = input._1
      val predicate = input._2
      val arguments = input._3
      val frame = output._1.get

      val lines = arguments.zip(frame.matchedArguments).collect {
        case (Some(argument), Some(matched)) if argument.semanticHead.isDefined =>
          val semHeadIndex = argument.semanticHead.get
          val semHeadWord = sentence.words(semHeadIndex)
          val groupType = matched.text
          val predicateWithFrame = frame.frame.frameText

          val lineParts = Seq(predicateWithFrame, predicate.orth, semHeadWord.base, semHeadWord.orth, groupType, caseWeight.toString)
          val line = lineParts.map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
          line
      }
      lines
    case _ => Seq.empty
  }

  println(counter.value, examples.value)
  resultCorpus.saveAsTextFile(outputPath)

  //val corpusOut = new PrintWriter(outputPath)
  //resultCorpus.collect().foreach(corpusOut.println)
  //corpusOut.close()

  //println(s"Total: $total, totalNotNone: $totalNotNone, easyCases: $easyCases")


//  def comparableFormat(x: String) = BracketAwareSplitter(':', x).last.replace(" ", "")
}
