package main.pas

import java.io.FileInputStream
import java.nio.file.Paths

import experimental.AnnotateSentencesFromXP._
import experimental.{BaseCSVToXCES, MultiLabelSparkHelper}
import kodie.phd.assigner.RulebasedPredicateAssigner
import kodie.phd.features._
import kodie.phd.formats.{PPAttachmentProbs, CCLSAXSentenceReader}
import kodie.phd.skladnica.features._
import kodie.phd.skladnica.types.Sentence
import kodie.phd.tools.crf.{CRF, LocalCRFDriver}
import org.apache.spark.{SparkContext, SparkConf}

object RunAnnotateWalentyExamples extends App {
  val outputPath = args(0)
  val modelPath = args(1).trim()
  val algorithm = args(2)
  val cost = args(3).toDouble
  val minFreq = args(4).toInt
  val labelProc = LabelProcessor.fromString(args(5))
  val keepLabels = args(6).toInt
  val keepLabelsThr = args(7).toDouble
  val useCpInt = args(8).toLowerCase == "true"
  val chunkedPath = args.lift(9).getOrElse("data/pantera_walenty_examples_20160205.txt.disamb.chunked")
  val csvPath = args.lift(10).getOrElse("data/walenty_detailed_examples_20160205.csv")

  println(s"Chunked PATH: $chunkedPath -- CSVPATH: $csvPath")

  AnnotateWalentyExamples.annotate(outputPath, modelPath, labelProc, keepLabels, keepLabelsThr, useCpInt, chunkedPath, csvPath)

}

object AnnotateWalentyExamples {
 def annotate(outputPath: String, modelPathStr: String, labelProcessor: LabelProcessor , keepLabels: Int = 3, useKeepLabelThreshold: Double = 0.6, useCpIntHeuristic: Boolean = true, chunkedPath: String, csvPath: String) = {
   val config = new SparkConf().setMaster("local[10]").setAppName("test-app").set("spark.executor.memory", "44g")
   //val outputPath = args.lift(1).getOrElse("data/pantera-new-multi-with-cp-heur-walentyExamples-with-args-and-ann.obj")

   //val sshCRFDriver = new RemoteCRFDriver("kodie@mozart")
   val sshCRFDriver = LocalCRFDriver // Local is OK for testing (the same command as Customized)

   val modelPath = Paths.get(modelPathStr).toAbsolutePath.toString
   val assigner = RulebasedPredicateAssigner
   assigner.keepNotConnected = true

   //val sents = CCLSAXSentenceReader.read("walentyExamples.xces.chunked", new FileInputStream("data/pantera-walenty-examples.txt.disamb.chunked"))
   val sents = CCLSAXSentenceReader.read(chunkedPath, new FileInputStream(chunkedPath))

   val CSVToXCES = new BaseCSVToXCES(csvPath)

   println(s"Num sentences: ${sents.size}; Num in csv: ${CSVToXCES.rows.size}")
   // Filtering out "Bład tagowania" sentences (that have no words!) and examples (new-corpus only) without schemax`
   // czy na pewno kolejnosc jest w porządku?
   val sentsWithRows = sents.zip(CSVToXCES.rows).filterNot(x => x._1.words.isEmpty || x._2.getOrElse("ramka", "brak") == "brak")
   println(s"sentsWithRows: ${sentsWithRows.size}")

   def onlyChars(s: String) = s.replace("&quot;", "\"").replaceAll("[^a-zA-Z0-9]", "")
   sentsWithRows.foreach { case (sent, row) =>
     if (onlyChars(sent.text) != onlyChars(row("przyklad"))) {
       println("Mismatch: ")
       println("\t" + sent.text)
       println("\t" + row("przyklad"))
     }
   }
   //println("Done!")
   //System.exit(0) // OK for now

   implicit val sc = new SparkContext(config)

   val sparkSentencesWithRows = sc.parallelize(sentsWithRows, 100)

   //println(sparkSentences.count())
   //println(rows.count())
   //println(sparkSentences.zip(rows).count())
   //println(sparkSentences.zip(rows).filter(_._1.words.size <= 100).count())
   //println(sparkSentencesWithRows.filter(_._1.words.size <= 100).count())

   val ppCases = PPAttachmentProbs.load("pp.in")
   val featureExtractors: Seq[FeatureExtractor[(Sentence, Int)]] = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _, wordToAspect _, wordToChunkHead _, predNumberMatch _, ppAttachmentNoun(ppCases, 0.01) _, predDistance("pp", Seq("prep")) _, predDistance("subst", Seq("subst", "psubst")) _, matchingCaseNearby _) map (x => Function.tupled(x))
   val conllWriter = new SkladnicaConllWriter(featureExtractors, extractNoArguments)

   val crf = new CRF(modelPath, sshCRFDriver)

     val sentenceWithArguments = sparkSentencesWithRows.filter(_._1.words.size <= 100).mapPartitions {
     case (sentencesWithAnn) =>
       val sentWithAnnSeq = sentencesWithAnn.toIndexedSeq
       val extras = sentWithAnnSeq.map(_._2)
       //println(sentWithAnnSeq.size)
       MultiLabelSparkHelper.extractFromSentences(sentWithAnnSeq.map(_._1), crf, conllWriter, assigner, keepLabels, useKeepLabelThreshold, useCpIntHeuristic, Some(labelProcessor)).iterator.zip(extras.iterator)
   }
   sentenceWithArguments.saveAsObjectFile(outputPath)

   println(s"STORED RESULT IN: $outputPath")
 }
}
