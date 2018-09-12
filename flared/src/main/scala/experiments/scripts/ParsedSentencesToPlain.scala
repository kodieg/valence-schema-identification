package experiments.scripts

import java.io.{FileInputStream, PrintWriter}

import kodie.phd.formats.CCLSentenceReader
import kodie.phd.skladnica.SkladnicaSentenceParser
import kodie.phd.utils.spark.Checkpointed
import org.apache.spark.SparkContext

/**
 * Outputs sentences in plain format needed for SkladnicaReplacePoSTagging
 */
object ParsedSentencesToPlain extends App {

  implicit val sc = new SparkContext("local", "test-app")
  val parsedSentencesPath = "parsed_sentences_all.dat"
  val outputPath = "data/ Skladnica.txt"

  val parsedSentences = Checkpointed(sc, parsedSentencesPath) {
    println("Rebuilding cache...")
    val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/*/*/*.xml")
    // val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/NKJP_1M_EkspressWieczorny/*/*.xml")
    val parsedSentences = sentencesFiles.flatMap { p => SkladnicaSentenceParser.parse(p._1, p._2) }
    parsedSentences.cache()
  }

  val outputWriter = new PrintWriter(outputPath)
  try {
    parsedSentences.map(_.text).foreach { sentenceText =>
      outputWriter.println(sentenceText)
    }
  } finally {
    outputWriter.close()
  }


}

/**
 * Replaces words and chunks in Skladnica using Pantera (or sth else) annotation
 */
object SkladnicaReplacePoSTagging extends App {
  implicit val sc = new SparkContext("local", "test-app")
  val parsedSentencesPath = "parsed_sentences_all.dat"
  val panteredSentences = "data/Skladnica.txt.disamb.chunked"  // Chunked and posed by pantera

  val parsedSentences = Checkpointed(sc, parsedSentencesPath) {
    println("Rebuilding cache...")
    val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/*/*/*.xml")
    // val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/NKJP_1M_EkspressWieczorny/*/*.xml")
    val parsedSentences = sentencesFiles.flatMap { p => SkladnicaSentenceParser.parse(p._1, p._2) }
    parsedSentences.cache()
  }

  val parsed = CCLSentenceReader.read("source", new FileInputStream(panteredSentences), true)

  //println("parsed: ", parsed.size)
  //println("rawSents: ", rawSents.size)
  //require(parsed.size == rawSents.size)o


  def toKey(s: String) = s.replaceAll("[^A-Za-z]", "").toLowerCase()


  val textToWords = (parsed).map(sent => toKey(sent.text) -> (sent.words, sent.chunks)).toMap
  val mapping = sc.broadcast(textToWords)

  val newSentences = parsedSentences.flatMap { sentence =>
    // I could think of some automatic moving of annotation indexes! instead of dropping sentences (but it is not that easy)
    // Wiekszość błędów dotyczy liczb podzielonych na wiecej niz 1 segment! (e.g. 1000 . 00 ) no i aglt "em" i rzeczy ala xxx-yyy
    mapping.value.get(toKey(sentence.text)) match {
      case Some((newWording, newChunks)) if newWording.size == sentence.words.size => Some(sentence.copy(words = newWording, chunks = newChunks))
      case None =>
        println(s"Not found! ${toKey(sentence.text)}")
        None
      case Some((k, _)) =>
        println("Different segmentation could lead to invalid arguments")
        println(k.map(_.orth).toSeq)
        println(sentence.words.map(_.orth).toSeq)
        None
    }
  }

  newSentences.saveAsObjectFile("data/Skladnica-full-pantera.dat")
}