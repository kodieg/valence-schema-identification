package main.pas

import java.io.{File, PrintWriter, FileInputStream}
import java.nio.file.{StandardCopyOption, Paths, Files}

import experimental.FeaturesSets
import kodie.phd.assigner.{RulebasedPredicateAssigner, PredicateAssigner}
import kodie.phd.features._
import kodie.phd.formats.{ConllReader, PPAttachmentProbs, CCLReader}
import kodie.phd.skladnica.{SentPFixer, SkladnicaConstants, SkladnicaSentenceParser}
import kodie.phd.skladnica.features._
import kodie.phd.skladnica.types.{Sentence, Word}
import kodie.phd.tools.crf.{CustomizedLocalCRFDriver, CRF, LocalCRFDriver}
import kodie.phd.utils.ml.DatasetUtils
import kodie.phd.utils.spark.Checkpointed
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Try


trait LabelProcessor extends Serializable {
  def simplify(index: Int, words: Seq[Word], label: String): String
  def recover(index: Int, words: Seq[Word], label: String): String
  def toShortString: String
  def toString: String
}

class ComplexLabelProcessor(all: Seq[LabelProcessor]) extends LabelProcessor {
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    all.foldLeft(label) { case (lab, proc) =>
      proc.simplify(index, words, lab)
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
      all.foldLeft(label) { case (lab, proc) =>
        proc.recover(index, words, lab)
      }
    }
  def toShortString: String = all.map(_.toShortString).mkString("")
  override def toString: String = all.map(_.toString).mkString(", ")
}

object LabelProcessor {
  def fromString(name: String): LabelProcessor = {
    val parts = name.split(",")

    if (parts.size > 1 ) new ComplexLabelProcessor(parts.map(fromString _).toSeq)
    else if (parts.size == 1) {

      parts(0).toLowerCase match {
        case "i" | "identity" => IdentityLabelProcessor
        case "p1" | "prep" => PrepProcessor
        case "p2" => Prep2Processor
        case "q1" => PrepAdjpProcessor
        case "q2" => Prepadjp2Processor
        case "s" => SentpProcessor
        case "n" => new OneArgProcessor("n", "np")
        case "a" => new OneArgProcessor("a", "adjp")
      }
    } else {
        throw new RuntimeException("Incorrect label processor setting!")
      }
    // TODO: combinations!
    //case "prepsentp" | "ps" => new ComplexLabelProcessor(Seq(PrepProcessor, SentpProcessor))
    //case "prep2sentp" => new ComplexLabelProcessor(Seq(Prep2Processor, SentpProcessor))
  }
}

object RecoveryBaseMapping {
  val recoveryMapping = Map(
    "iż" -> "że",
    "aby" -> "żeby",
    "gdy" -> "kiedy",
    "skoro" -> "ponieważ",
    "choć" -> "chociaż",
    "jeżeli" -> "jeśli"
  )
}

object PrepAdjpProcessor extends LabelProcessor {
  def main(args: Array[String]) = {
    val word = Word("po", "po", "po:loc")
    val label = "prepnp(po,dop)"
    val z = new ComplexLabelProcessor(Seq(PrepProcessor, SentpProcessor))
    val simpl = z.simplify(0, Seq(word), label)
    val rec = z.recover(0, Seq(word), simpl)

    println(simpl)
    println(rec)
  }
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepadjp")) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim()/*.replaceAll(",", "")*/)
        s"$lbl(_,${args(1)})"
      }.getOrElse {
        println(s"PREPPROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepadjp")) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
      val base = words(index).base.trim()
      val recovered = RecoveryBaseMapping.recoveryMapping.getOrElse(base, base)
      s"$lbl(${recovered},${args(1)})"
    } else {
      label
    }
  }
  def toShortString: String = "Q"
  override def toString: String = "Prepadjp1"
}

object Prepadjp2Processor extends LabelProcessor {
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepadjp")) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
        s"$lbl(${args(0)}},_)"
      }.getOrElse {
        println(s"PREP 2 PROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepadjp")) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
      val cas = words(index).ctag.split(":").drop(1).headOption.map(c => SkladnicaConstants.REVCASES.getOrElse(c, "unknown")).getOrElse("unknown")
      val base = words(index).base.trim()
      val recovered = RecoveryBaseMapping.recoveryMapping.getOrElse(base, base)
      s"$lbl(${args(0)},$cas)"
    } else {
      label
    }
  }
  def toShortString: String = "Q2"
  override def toString: String = "Prepadjp2"
}


object PrepProcessor extends LabelProcessor {
  def main(args: Array[String]) = {
    val word = Word("po", "po", "po:loc")
    val label = "prepnp(po,dop)"
    val z = new ComplexLabelProcessor(Seq(PrepProcessor, SentpProcessor))
    val simpl = z.simplify(0, Seq(word), label)
    val rec = z.recover(0, Seq(word), simpl)

    println(simpl)
    println(rec)
  }
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepnp") || label.startsWith("prepadjp")) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim()/*.replaceAll(",", "")*/)
        s"$lbl(_,${args(1)})"
      }.getOrElse {
        println(s"PREPPROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepnp") || label.startsWith("prepadjp")) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
      val base = words(index).base.trim()
      val recovered = RecoveryBaseMapping.recoveryMapping.getOrElse(base, base)
      s"$lbl(${recovered},${args(1)})"
    } else {
      label
    }
  }
  def toShortString: String = "P"
  override def toString: String = "Prepositions1"
}

object Prep2Processor extends LabelProcessor {
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepnp") || label.startsWith("prepadjp")) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
        s"$lbl(${args(0)}},_)"
      }.getOrElse {
        println(s"PREP 2 PROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("prepnp") || label.startsWith("prepadjp")) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val args = label.substring(argStart + 1, label.lastIndexOf(')')).split(",").map(_.trim())
      val cas = words(index).ctag.split(":").drop(1).headOption.map(c => SkladnicaConstants.REVCASES.getOrElse(c, "unknown")).getOrElse("unknown")
      val base = words(index).base.trim()
      val recovered = RecoveryBaseMapping.recoveryMapping.getOrElse(base, base)
      s"$lbl(${args(0)},$cas)"
    } else {
      label
    }
  }
  def toShortString: String = "P2"
  override def toString: String = "Prepositions2"
}

object Test extends  App{
  val np = new OneArgProcessor("np", "np")
  println(np.simplify(0, Seq(Word("pies", "pies"," subst:acc:xx:xx")), "np(acc)"))
  println(np.recover(0, Seq(Word("pies", "pies"," subst:acc:xx:xx")), "np(_)"))
}

class OneArgProcessor(short: String, long: String) extends LabelProcessor {
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith(long)) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).trim()

        s"$lbl(_)"
      }.getOrElse {
        println(s"$long PROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith(long)) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val cas = words(index).ctag.split(":").drop(1).headOption.map(c => SkladnicaConstants.REVCASES.getOrElse(c, "unknown")).getOrElse("unknown")

      s"$lbl(${cas})"
    } else {
      label
    }
  }
  def toShortString: String = short.toUpperCase
  override def toString: String = long

}

object SentpProcessor extends LabelProcessor {
  def main(args: Array[String]) = {
    val word = Word("że", "że", "po:loc")
    val label = "sentp(że)"
    val simpl = simplify(0, Seq(word), label)
    val rec = recover(0, Seq(word), simpl)

    println(simpl)
    println(rec)
  }
  def simplify(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("sentp")) {
      Try {
        val argStart = label.indexOf('(')
        val lbl = label.substring(0, argStart)
        val args = label.substring(argStart + 1, label.lastIndexOf(')')).trim()

        if (args != "pz") {
          if (args != words(index).base) {
            println(s"SENTP MISMATCH $label --> ${words(index).base}")
          }
          s"$lbl(_)"
        } else
          label
      }.getOrElse {
        println(s"SENT PROCESSOR FAILED FOR: $label")
        label
      }
    } else {
      label
    }
  }
  def recover(index: Int, words: Seq[Word], label: String): String = {
    if (label.startsWith("sentp")) {
      val argStart = label.indexOf('(')
      val lbl = label.substring(0, argStart)
      val args = label.substring(argStart + 1, label.lastIndexOf(')')).trim()
      if (args != "pz") {
        val base = words(index).base.trim()
        val recovered = RecoveryBaseMapping.recoveryMapping.getOrElse(base, base)

        s"$lbl(${recovered})"
      } else
        label
    } else {
      label
    }
  }
  def toShortString: String = "S"
  override def toString: String = "Sentence"
}

object IdentityLabelProcessor extends LabelProcessor {
  def simplify(index: Int, words: Seq[Word], label: String): String = label
  def recover(index: Int, words: Seq[Word], label: String): String = label
  def toShortString: String = "I"
  override def toString: String = "Identity"
}

object RunCRFGridEvaluation extends App {
  val featureDesc = args(0)
  val featureFile = args(1)
  val algorithm = args(2)
  val cost = args(3).toDouble
  val minFreq = args(4).toInt
  val labelProc = LabelProcessor.fromString(args(5))
  val tagger = args.lift(6).getOrElse("pantera")
  val doCV = args.lift(7).getOrElse("true") == "true"
  val outputPath = args.lift(8)
  val inputPath = args.lift(9)
  /*val keepLabels = args(6).toInt
  val keepLabelsThr = args(7).toDouble
  val useCpInt = args(8).toLowerCase == "true"*/

  val features = Source.fromFile(featureFile).getLines().mkString("\n")

  CRFGridEvaluation.evaluate(featureDesc, features, algorithm, cost, minFreq, labelProc, tagger, doCV, outputPath, inputPath)
}

object CRFGridEvaluation {
  /*
      algorithm = CRF or L1-CRF
   */
  def evaluate(featuresDesc: String, features: String, algorithm: String, costParameter: Double, minFreq: Int, labelProcessor: LabelProcessor, tagger: String = "pantera", doCV: Boolean=false, outputPath: Option[String]=None, inputPath: Option[String]=None) = {
    val baseDir = outputPath.map(new File(_).getParent).getOrElse(s"data/crf-grid-eval/crf-$featuresDesc-a-$algorithm-c-$costParameter-minFreq-$minFreq-label-${labelProcessor.toShortString}${tagger.replace("pantera", "")}")
    val bd = new File(baseDir)
    bd.mkdirs()
    val outputModelPath = outputPath.getOrElse(s"$baseDir/model.crf")
    if (new File(outputModelPath).exists()) {
      println(s"MODEL ALREADY EXISTS: $outputModelPath")
      println(s"MODEL SAVED IN @$outputModelPath@") // USED by run-big-experiment.sh
      System.exit(0)
    }
    val reportPath = s"$baseDir/cross-validation-report.txt"
    val sshCRFDriver = new CustomizedLocalCRFDriver(algorithm, costParameter, minFreq) // new RemoteCRFDriver("kodie@mozart")
    val parsedSentencesPath = "parsed_sentences_all.dat_new"
    val chunkedSentencesPath = inputPath.getOrElse(if (tagger == "pantera") "data/Skladnica-full-pantera.dat" else "chunked_sentences.dat_new")

    implicit val sc = new SparkContext("local", "test-app")
    val startTime = System.nanoTime()


    //CCLWriter.write(parsedSentences.collect(), new PrintWriter(new FileOutputStream("corpus.ccl.xml")))
    val chunkedSentences: RDD[Sentence] = Checkpointed(sc, chunkedSentencesPath) {
      val parsedSentences = Checkpointed(sc, parsedSentencesPath) {
        println("Rebuilding cache...")
        val sentencesFiles = sc.wholeTextFiles("/home/kodie/zrobione131011/*/*/*.xml") //on mozart
        // val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/NKJP_1M_EkspressWieczorny/*/*.xml")
        val parsedSentences = sentencesFiles.flatMap { p => SkladnicaSentenceParser.parse(p._1, p._2) }
        parsedSentences.cache()
      }
      println("sentences done!  ")

      val chunks = CCLReader.read(new FileInputStream("corpus.ccl.xml.chunked"))
      val sents = parsedSentences.map {
        sentence =>
          sentence.copy(chunks = Some(chunks(sentence.source.replace("/home/kodie/", "/Users/kodie/Downloads/")).toSeq))
      }
      sents.cache()
      sents
    }.map(SentPFixer.fixSentP _)

    val ppCases = PPAttachmentProbs.load("pp.in")


    val featureExtractors: Seq[FeatureExtractor[(Sentence, Int)]] = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _, wordToAspect _, wordToChunkHead _, predNumberMatch _, ppAttachmentNoun(ppCases, 0.01) _, predDistance("pp", Seq("prep")) _, predDistance("subst", Seq("subst", "psubst")) _, matchingCaseNearby _) map (x => Function.tupled(x))
    val conllWriter = new SkladnicaConllWriter(featureExtractors, extractBothArgumentTypesAndAdjuncts)
    val conllDataset = chunkedSentences.map(s => (s, conllWriter.extract(s)))
    val localDataset = conllDataset.collect()

    val foldedDataset = DatasetUtils.folds(10, localDataset)

    val featuresSets = FeaturesSets.allWithB
    val out = new PrintWriter(reportPath)
    out.println(s"Features: $featuresDesc")
    out.println(s"Algorithm: $algorithm")
    out.println(s"Cost: $costParameter")
    out.println(s"Min freq: $minFreq")
    out.println(s"Label processor: ${labelProcessor.toString}")

    def simplifyLabel(sentence: Sentence, labels: Array[String]): Array[String] = {
      labels.zipWithIndex.map {
        case (l, i) => labelProcessor.simplify(i, sentence.words, l)
      }
    }

    def recoverLabels(sentences: Seq[Sentence], predictionResults: Stream[String]): Stream[String] = {
      val bySent = ConllReader.bySentence(predictionResults, -1, true)

      val result = sentences.zip(bySent).flatMap { case (sent, args) =>
        val line = args.zipWithIndex.map { case (labelAndFeats, idx) =>
          val lbIdx = labelAndFeats.size - 1
          labelAndFeats(lbIdx) = labelProcessor.recover(idx, sent.words, labelAndFeats(lbIdx))
          labelAndFeats.mkString("\t")
        }.toStream
        line ++ Stream("")
      }.toStream

      result
    }

    if (doCV) {
      val crfResults = foldedDataset.map {
        case (train, test) =>
          val model = CRF.train(features, train.map(d => simplifyLabel(d._1, d._2)), sshCRFDriver)
          val predictionResults = recoverLabels(test.map(_._1), model.predict(test.map(_._2)))
          val stats = ConllReader.calculateStats(predictionResults)
          val predictedArgumentsPerSentence = ConllReader.bySentence(predictionResults).map(_.map(_.last))
          val testWithArgs = test.map(_._1).zip(predictedArgumentsPerSentence)
          val assignStats = PredicateAssigner.evaluate(RulebasedPredicateAssigner, testWithArgs)

          println("CRF", stats, "assign", assignStats)
          (stats, assignStats)
      }

      val results = Array.fill(5)(ArrayBuffer[Double]())

      for ((line, i) <- crfResults.zipWithIndex) {
        out.println(s"${i + 1} CRF prec.: ${line._1._1}; CRF rec.: ${line._1._2}; CRF+Assign prec. ${line._2._1._1}; CRF+Assign rec. ${line._2._1._2}; CRF+Assign Sent: ${line._2._2}")
        results(0) += line._1._1
        results(1) += line._1._2
        results(2) += line._2._1._1
        results(3) += line._2._1._2
        results(4) += line._2._2
      }

      def mean(x: Seq[Double]) = x.sum / x.size

      def std(x: Seq[Double]) = {
        val m = mean(x)
        x.map(_ - m).map(x => x * x).sum / x.size
      }

      out.println("\nOverall results:")
      out.println(s"CRF PRECISION: ${mean(results(0))} +- ${std(results(0))}")
      out.println(s"CRF RECALL: ${mean(results(1))} +- ${std(results(1))}")
      out.println(s"CRF+ASSIGN PRECISION: ${mean(results(2))} +- ${std(results(2))}")
      out.println(s"CRF+ASSIGN RECALL: ${mean(results(3))} +- ${std(results(3))}")
      out.println(s"CRF+ASSIGN SENT ACCURACY: ${mean(results(4))} +- ${std(results(4))}")

      out.flush()
      out.close()

    }

    val model = CRF.train(features, localDataset.map(_._2), sshCRFDriver)
    Files.copy(Paths.get(model.modelPath), Paths.get(outputModelPath), StandardCopyOption.REPLACE_EXISTING)

    println(s"MODEL SAVED IN @$outputModelPath@")
  }
}
