package experimental

import kodie.phd.features
import kodie.phd.skladnica.{NonchHeuristic, SiebieHeuristic, PrepJakoHeuristic, CpIntHeuristic}
import main.pas.LabelProcessor

import scala.collection.mutable
import kodie.phd.features._
import kodie.phd.skladnica.features._
import kodie.phd.skladnica.types.{Span, Sentence}
import kodie.phd.formats.{CCLSAXSentenceReader, CCLSentenceReader, ConllReader, PPAttachmentProbs}
import kodie.phd.tools.crf.{LocalCRFDriver, CRF, RemoteCRFDriver}
import java.nio.file.Paths
import kodie.phd.assigner.{AbstractRulebasedPredicateAssigner, SemanticHeadAssigner, PredicateAssigner, RulebasedPredicateAssigner}
import org.apache.spark.{SparkConf, SparkContext}
import scala.util.{Try, Random}
import java.io.{File, FileInputStream}

/** TODO: this needs to be moved out of experimental and refactored */
object ExtractStructureFromCCL extends App {
  val config = new SparkConf().setMaster("local[20]").setAppName("test-app").set("spark.executor.memory", "34g")
  implicit val sc = new SparkContext(config)
  // old name was sentence-with-args.obj!
//  val outputPath = args.lift(1).getOrElse("/home/kodie/Flared/data/pantera-nkjp-300m-sentence-with-multi-args.obj")
  val outputPath = args.lift(1).getOrElse("/home/kodie/Flared/data/pantera-nkjp-300m-sentence-CRF-c-0.5-keep-2-thr-0.9.obj")

  // val sshCRFDriver = new RemoteCRFDriver("kodie@mozart")
  val sshCRFDriver = LocalCRFDriver
  // TODO: when running on mozart use Local and there'll be no problem with sending model! :)
  // Actually I'd need to send this model to remote server! If use RemoteCRFDriver!
  val modelPath = Paths.get(args.lift(0).getOrElse("data/crf-grid-eval/crf-all.txt-a-CRF-c-0.5-minFreq-1-label-P-with-lexsiebiecp/model.crf")).toAbsolutePath.toString
//  val modelPath = Paths.get("data/pantera-final-model.crf").toAbsolutePath.toString
  val assigner = RulebasedPredicateAssigner
  val files = sc.textFile("data/input-files", 75000)

  val sentences = files.flatMap(filePath => CCLSentenceReader.read(filePath, new FileInputStream(filePath)))
  val sparkSentences = sentences.setName("sentences").filter(s => ! s.words.isEmpty)

  // val sentences = CCLSentenceReader.read("test", getClass.getResourceAsStream("/ccl-sample.xml"))
  // val sparkSentences = sc.parallelize(sentences)


  val ppCases = PPAttachmentProbs.load("pp.in")
  val featureExtractors: Seq[features.FeatureExtractor[(Sentence, Int)]] = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _, wordToAspect _, wordToChunkHead _, predNumberMatch _, ppAttachmentNoun(ppCases, 0.01) _, predDistance("pp", Seq("prep")) _, predDistance("subst", Seq("subst", "psubst")) _, matchingCaseNearby _) map (x => Function.tupled(x))
  val conllWriter = new SkladnicaConllWriter(featureExtractors, extractNoArguments)

  val crf = new CRF(modelPath, sshCRFDriver)

  val sentenceWithArguments = sparkSentences.filter(_.words.size <= 100).mapPartitions {
    case sentences =>
      val sentencesSeq = sentences.toSeq
      if (sentencesSeq.nonEmpty) {
        //SparkHelper.extractFromSentences(sentences.toSeq, crf, conllWriter, assigner).iterator
        MultiLabelSparkHelper.extractFromSentences(sentences.toSeq, crf, conllWriter, assigner, 2, 0.9, true, Some(LabelProcessor.fromString("prep"))).iterator
      } else {
        Iterator.empty
      }
  }
  sentenceWithArguments.saveAsObjectFile(outputPath)
}

case class Argument(argumentType: String,  predicate: Span, semanticHead: Option[Int])
case class ArgumentWithProb(argumentType: String,  predicate: Span, semanticHead: Option[Int], prob: Double)

object SparkHelper {
  def extractFromSentences(sentences: Seq[Sentence], crf: CRF, conllWriter: SkladnicaConllWriter, assigner: PredicateAssigner) = {
    val dataset: Seq[(Sentence, Array[String])] = sentences.map(s => (s, conllWriter.extract(s)))
    val seq = Seq()
    val randomPart  = Random.alphanumeric take(30) mkString("")

    val datasetName = s"/home/kodie/tmp/dataset-${randomPart}.test"
//    val datasetName = s"dataset-${randomPart}.test"
    println(s"Storing dataset and launching CRF for $datasetName")
    val predictedLabels = crf.predict(dataset.map(_._2), datasetName)
    val predictedBySentence = ConllReader.bySentence(predictedLabels)

    println(sentences.size)
    println(predictedBySentence.size)

    val sentenceWithLabels = dataset.map(_._1).zip(predictedBySentence)

    println(s"Assigning heads to the sentences from $datasetName")
    val result = sentenceWithLabels.map {
      case (sentence, rawLabels) =>
        val labels = rawLabels.map(_.last)
        if (sentence.words.size != labels.size) {
          println("Error! Labels and words sizes are different")
          println(s"Words: ${sentence.words.toList}")
          println(s"Labels: ${labels.toList}")
          println(s"Data taken from: ${sentence.source}")
          println(s"Text: ${sentence.text}")
          throw new Exception(s"Invalid sentence: $sentence")
        }
        val words = sentence.words.toIndexedSeq

        try {

          val args = assigner.assign(words, labels)
          val synHeads = SemanticHeadAssigner.assignToArgs(words, labels)

          val argsWithHeads = args.zip(synHeads).map {
            case (Some((predicate, argType)), maybeHead) => Some(Argument(argType, predicate, maybeHead))
            case _ => None
          }

          //println(sentence.text, argsWithHeads)
          (sentence, argsWithHeads) // TODO: do something with args
        } catch {
          case e: ArrayIndexOutOfBoundsException =>
            println(s"Words: ${words.size} : $words")
            println(s"Words with index ${words.zipWithIndex}")
            println(s"Arguments: ${labels}")
            throw e
        }

            }
    Try(new File(datasetName).renameTo(new File(datasetName + "-done")))
    println(s"Dataset: $datasetName done!")
    result
  }
}

object MultiLabelSparkHelper {
  def extractFromSentences(sentences: Seq[Sentence], crf: CRF, conllWriter: SkladnicaConllWriter, assigner: AbstractRulebasedPredicateAssigner, keepLabels: Int = 3, useKeepLabelThreshold : Double = 0.6, useCpIntHeuristic: Boolean = true, labelProc: Option[LabelProcessor]=None, usePrepJakoHeuristic: Boolean = true, useSiebieHeuristic: Boolean = false, useNonchHeuristic: Boolean = true) = {
    def sortAndFilterLabels(labels: Array[Array[String]]) = {
      labels.map { possibleLabels =>
        val labelsWithWeigtsSet = possibleLabels.map { labWithWeight =>
          val parts = labWithWeight.split("/")
          parts(0) -> parts(1).toDouble
        } toSet

        val labelsWithWeights = labelsWithWeigtsSet.toSeq.sortBy(-_._2)
        if (labelsWithWeights.lift(0).getOrElse(("_", 1.0))._2 > useKeepLabelThreshold)
          (labelsWithWeights ++ Array(("_", 1.0))).take(1)
        else {
          // taking max if few labels are repeated (maybe sum would be better, but on the other hand sum could be greater than 0.6)
          // maybe this operation should be performed eariler with sum?
          // Sometimes however I've seen duplicate "_" labels with 0.9
          val z = labelsWithWeights.filter(_._1 != "_").groupBy(_._1).map(p => p._1 -> p._2.map(_._2).max)
          z.toSeq.sortBy(-_._2).take(keepLabels)

          // below is original
          //labelsWithWeights.filter(_._1 != "_").take(keepLabels)
        }
      }
    }
    val dataset: Seq[(Sentence, Array[String])] = sentences.map(s => (s, conllWriter.extract(s)))
    val seq = Seq()
    val randomPart  = Random.alphanumeric take(30) mkString("")

    val datasetName = s"/home/kodie/tmp/dataset-${randomPart}.test"
    //    val datasetName = s"dataset-${randomPart}.test"
    println(s"Storing dataset and launching CRF for $datasetName")
    val predictedLabels = crf.predict(dataset.map(_._2), datasetName, true)
    val numFeats = conllWriter.featureExtractors.size + 1
    println(s"numFeats: $numFeats")
    val predictedBySentence = ConllReader.bySentence(predictedLabels, numFeats, true)

    println(sentences.size)
    println(predictedBySentence.size)

    val sentenceWithLabels = dataset.map(_._1).zip(predictedBySentence)

    println(s"Assigning heads to the sentences from $datasetName")
    val result = sentenceWithLabels.map {
      case (sentence, rawLabels) =>
        val sortedLabels0 = sortAndFilterLabels(rawLabels)
        val sortedLabels = labelProc.map { proc =>
          sortedLabels0.zipWithIndex.map { case (sortLab, idx) =>
            sortLab.map(x => proc.recover(idx, sentence.words, x._1) -> x._2)
          }
        }.getOrElse(sortedLabels0)
        val labels0 = if (useCpIntHeuristic) CpIntHeuristic.fillMissing(sentence.words, sortedLabels) else sortedLabels
        val labels1 = if (usePrepJakoHeuristic) PrepJakoHeuristic.fillMissing(sentence.words, labels0) else labels0
        val labels2 = if (useSiebieHeuristic) SiebieHeuristic.fillMissing(sentence.words, labels1) else labels1
        val labels = if (useNonchHeuristic) NonchHeuristic.fillMissing(sentence.words, labels2) else labels2
        if (sentence.words.size != labels.size) {
          println("Error! Labels and words sizes are different")
          println(s"Words: ${sentence.words.toList}")
          println(s"Labels: ${labels.toList}")
          println(s"Data taken from: ${sentence.source}")
          println(s"Text: ${sentence.text}")
          throw new Exception(s"Invalid sentence: $sentence")
        }
        val words = sentence.words.toIndexedSeq

        try {

          val baseLabels = labels.map(_.head._1)
          val args = assigner.assign(words, baseLabels)
          val synHeads = SemanticHeadAssigner.assignToArgs(words, baseLabels)

          val argsWithHeads = args.zip(synHeads).zip(labels.map(_.head._2)).map {
            case ((Some((predicate, argType)), maybeHead), prob) => mutable.ArrayBuffer(ArgumentWithProb(argType, predicate, maybeHead, prob))
            case _ => mutable.ArrayBuffer[ArgumentWithProb]()
          }

          for ((labelSeq, i) <- labels.zipWithIndex) {
            if (labelSeq.size > 1) {
              val newLabels = baseLabels.clone()
              for (label <- labelSeq.view.drop(1)) {
                newLabels(i) = label._1
                val newArgs = assigner.assign(words, baseLabels)
                val newSynHeads = SemanticHeadAssigner.assignToArgs(words, baseLabels)

                newArgs(i).foreach { case (predicate, argType) =>
                  argsWithHeads(i) += ArgumentWithProb(argType, predicate, newSynHeads(i), label._2)
                }
              }
            }
          }

          //println(sentence.text, argsWithHeads)
          (sentence, argsWithHeads) // TODO: do something with args
        } catch {
          case e: ArrayIndexOutOfBoundsException =>
            println(s"Words: ${words.size} : $words")
            println(s"Words with index ${words.zipWithIndex}")
            println(s"Arguments: ${labels}")
            throw e
        }

    }
    Try(new File(datasetName).renameTo(new File(datasetName + "-done")))
    println(s"Dataset: $datasetName done!")
    result
  }
}

