package main.pas

import experimental.{MLCorpusToScikitCSVHelper, Argument, ArgumentWithProb}
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceManager, ServiceContext}
import kodie.phd.skladnica.types.{Word, Sentence}
import kodie.phd.walenty._
import main.pas.EvaluateFrameMatchingWith._
import org.apache.spark.{HashPartitioner, SparkConf, SparkContext}
import phd.sr.data.PredicateGroupDataset
import phd.sr.tools.utils.{common, GlobalConfiguration}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


/**
 * Created by kodie on 8/11/15.
 */
object ExtractCorpusForML extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }

  val (newArgs1, algosStr) = if (newArgs0(0) == "--algo") {
    (newArgs0.drop(2), newArgs0(1))
  } else {
    (newArgs0, "max:false")
  }


  val (newArgs, outputPathOpt) = if (newArgs1(0) == "--output") {
    (newArgs1.drop(2), Some(newArgs1(1)))
  } else {
    (newArgs1, None)
  }



  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }


  val config = new SparkConf().setMaster("local[60]").setAppName("test-app").set("spark.network.timeout", "1000000000").set("spark.executor.memory", "50g").set("spark.driver.maxResultSize", "20g")
  implicit val sc = new SparkContext(config)
  import org.apache.spark.SparkContext._


  //val inputPath = ""
  val inputPath = path.getOrElse("data/args-nkjp300m/pantera-nkjp300m-crf-l1-c-1.0-I.obj")
  val outputPath = outputPathOpt.getOrElse("data/pantera-nkjp-300m-max-maxnoxp-ml-dataset.obj")

//  val data = sc.objectFile[(Sentence, Seq[Option[Argument]])](inputPath).cache()
  val data = sc.objectFile[(Sentence, Seq[ArrayBuffer[ArgumentWithProb]])](inputPath)/*.sample(false, 0.00001)*/.coalesce(10000)


  val walenty = NewWalentyParser()
  val maxFrameChooser = new MaximalFrameChooser(walenty,
    // TrueFilterer
    new AndFilterer(new MinArgsMatchedFilterer(2), new AndFilterer(InfFilterer, LexicalizedFilterer))
  )
  val multiFrameChooser = new MultiFrameChooser(walenty, new AndFilterer(InfFilterer, LexicalizedFilterer))
  val bcFrameChooser = sc.broadcast(maxFrameChooser)
  val bcMultiFrameChooser = sc.broadcast(multiFrameChooser)


  val chooserCascade = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent).fromString(algosStr)

/*
  val chooserCascade = new FrameChooserCascade(List(
    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
      maxFrameChooser.chooseMaximalFrame(predicateBase, aspect, words, skladnicaArguments)

    /*    (predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedArgs: Seq[ArrayBuffer[ArgumentWithProb]]) =>
          srVoteFrameChooser.chooseFrame(predicateBase, aspect, words, skladnicaArguments),*/
  ))*/

  val counter = sc.accumulator(0, "counter")
  val examples = sc.accumulator(0, "examples")

  /*val easyCases = sc.accumulator(0)
  val totalNotNone = sc.accumulator(0)*/
  //val predicateErrors = sc.accumulator(0)

  val dataWithPredicates = data.flatMap {
    case (sentence, args) =>
      //val predicateIndexes = args.flatten.map(_.predicate.left).toSet
      val predicateIndexes = args.flatten.map(_.predicate.left).toSet.toSeq.sorted
      val cases = predicateIndexes.map { predicateIndex =>
        val leftNeighbour =  Try(predicateIndexes.filter(_ < predicateIndex).max).toOption
        val rightNeighbour = Try(predicateIndexes.filter(_ > predicateIndex).min).toOption

        val leftNeighIndex = leftNeighbour.getOrElse(0)
        val rightNeighIndex = rightNeighbour.getOrElse(sentence.words.size)

        val predicateWord = sentence.words(predicateIndex)
        val arguments = args.map {
          case someArgs/*@Some(arg)*/ => someArgs.filter(arg => arg.predicate.left == predicateIndex) // => someArg

        }
        val extendedArgs = args.zipWithIndex.map(pair => pair._1.filter { arg =>
          arg.predicate.left == predicateIndex || (pair._2 >= leftNeighIndex && pair._2 <= rightNeighIndex) } )
        (sentence, predicateWord, arguments, extendedArgs)
      }

      cases
  }



  val results = dataWithPredicates.flatMap {
    case input@(sentence, predicate, predicateArgs, extendedArgs) =>
      examples += 1
      val base = predicate.base
      val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"

      val output: (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo, Int) = chooserCascade.chooseFrame(base, aspect, sentence.words, predicateArgs, extendedArgs)

      val predicted: Option[MaximalFrameChooserResult] = output._1

      predicted.map { frame =>
        (sentence, predicate, predicateArgs, extendedArgs, frame.frame)
      }
  }// .filter(x => !x._5.arguments.exists(_.realizations.exists(_.startsWith("lex")))) // throw out "lex" frames!

  val r = results.keyBy(x => x._2.base + x._5.frameText).repartitionAndSortWithinPartitions(new HashPartitioner(20000)).cache()
  // Not save .... but dump csv
  r.saveAsObjectFile(outputPath)

  //MLCorpusToScikitCSVHelper.compute(sc, r, globalConfig, manager)
  println(s"EXAMPLES: ${examples.value}")


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
  /*

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

*/
  //val corpusOut = new PrintWriter(outputPath)
  //resultCorpus.collect().foreach(corpusOut.println)
  //corpusOut.close()

  //println(s"Total: $total, totalNotNone: $totalNotNone, easyCases: $easyCases")


//  def comparableFormat(x: String) = BracketAwareSplitter(':', x).last.replace(" ", "")
}
