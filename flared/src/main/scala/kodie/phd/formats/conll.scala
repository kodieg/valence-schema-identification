package kodie.phd.formats

import kodie.phd.features.{Feature, FeatureExtractor}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ConllWriter[T](featureExtractors: Iterable[FeatureExtractor[T]], resultExtractor: FeatureExtractor[T]) extends Serializable {
  val separator = ""

  def extract(instance: T) : String = {
    val features = featureExtractors.iterator.map(_(instance).toString)
    val result = resultExtractor(instance).toString

    line(features ++ Iterator.single(result))
  }

  private def line(columns: Iterator[Feature]) = columns.map(checkFeature).mkString("\t")

  private def checkFeature(f: Feature) : Feature = {
    if (f.contains("\t") || f.contains("\n") || f.contains(" ")) {
      throw new RuntimeException(s"Feature: '$f' contains invalid whitespace!")
    }
    return f
  }
}

object ConllReader {
  def extract(line: String): Option[Array[String]] = {
    if (line != "") Some(line.split("\t")) else None
  }



  def bySentence(output: Stream[String], numFeats: Int = -1, useV2: Boolean = false) : Array[Array[Array[String]]] = {
    def computeLabels(line: Array[String]) = line.takeRight(line.size - numFeats)
    var builder = ArrayBuffer[Array[String]]()
    val sentences = ArrayBuffer[Array[Array[String]]]()

    for (line <- output.filterNot(x => x.trim.startsWith("# ") && !x.contains("\t"))) {
      extract(line) match {
        case Some(features) => builder += (if (useV2) computeLabels(features) else Array(features.last))
        case None =>
          if (!builder.isEmpty) sentences += builder.toArray
          builder = ArrayBuffer()
      }
    }
    if (!builder.isEmpty) sentences += builder.toArray
    sentences.toArray
  }

  def calculateStats(classifierOutput: Stream[String]) = {
    var correctForPrecision = 0
    var totalForPrecision = 0
    var correctForRecall = 0
    var totalForRecall = 0

    for (line <- classifierOutput) {
      val columns = extract(line)
      val classes = columns.map(f => (f(f.size - 2), f.last))
      classes match {
        case Some((gold, predicted)) =>
          if (predicted != "_") {
            totalForPrecision += 1
            correctForPrecision += (if (gold == predicted) 1 else 0)
          }
          if (gold != "_") {
            totalForRecall += 1
            correctForRecall += (if (gold == predicted) 1 else 0)
          }

        case None =>
      }
    }
    (correctForPrecision.toDouble / totalForPrecision, correctForRecall.toDouble / totalForRecall)
  }
  
  def extractPredictedClasses(classifierOutput: Stream[String]) = {
    val predictedClassesStream = classifierOutput.map((extract _) andThen(_.map(_.last)))
    val aggregator = ListBuffer[ListBuffer[String]](ListBuffer[String]())
    val predictionsBySentence =  predictedClassesStream.foldLeft(aggregator) {
      case (buffer, Some(predictedClass)) => (buffer.last) += predictedClass; buffer
      case (buffer, None) => buffer += new ListBuffer[String]
    }
    predictionsBySentence.collect {
      case sentence if sentence.size > 0 => sentence.toArray
    }.toArray
  }
}