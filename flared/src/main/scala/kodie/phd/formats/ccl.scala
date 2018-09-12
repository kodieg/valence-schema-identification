package kodie.phd.formats

import kodie.phd.skladnica.types.Sentence
import java.io.{InputStream, PrintWriter}
import org.apache.commons.lang3.StringEscapeUtils

import scala.xml.{Node, XML}
import scala.xml.pull._
import kodie.phd.skladnica.types._
import kodie.phd.skladnica.SkladnicaXMLImplicits._
import scala.io.{Codec, Source}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.xml.dtd.{ParameterEntityDecl, IntDef, ParsedEntityDecl, EntityDecl}


/**
 * Created by kodie on 10/14/14.
 */
object CCLWriter {

  def write(sentences: Traversable[Sentence], output: PrintWriter) {
    output.write(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<!DOCTYPE chunkList SYSTEM "ccl.dtd">
        |<chunkList>
        |<chunk id="ch1" type="p">
      """.stripMargin)
    sentences.foreach(write(_, output))
    output.write(
      """
        | </chunk>
        |</chunkList>""".stripMargin)
    output.flush()
  }

  def write(sentence: Sentence, output: PrintWriter) {
    output.write(s"<sentence id='${sentenceId(sentence)}'>")
    for (word <- sentence.words) {
      output.write(s"<tok>\n<orth>${word.orth}</orth>\n")
      output.write(s"<lex disamb='1'><base>${word.base}</base><ctag>${fixCtag(word.ctag)}</ctag></lex>\n")
      output.write("</tok>\n")
    }
    output.write("</sentence>")
  }

  def fixCtag(ctag: String) = {
    val mappings = Map("padv" -> "adv", "padj" -> "adj", "psubst"->"subst")
    var result = ctag
    for (kv <- mappings) {
      result = result.replace(kv._1, kv._2)
    }
    result
  }

  def sentenceId(sentence: Sentence) = s"${sentence.source}"
}

// This implementation might be broken1
object CCLSAXSentenceReader {
  case class CCLChunk(index: Int, channel: String, number: Int, head: Boolean)
  case class CCLSentence(words: ArrayBuffer[Word], chunks: ArrayBuffer[CCLChunk])

  final val STORE_NODES = Set("orth", "base", "ctag")

  def read(source: String, input: InputStream) : IndexedSeq[Sentence] = {
      val sentences = readXML(input)

      val result = sentences.map(cclToSentence(source, _)) toArray

      result
  }

  def cclToSentence(source: String, cclSentence: CCLSentence) = {
    def wordToWordWithSep(word: Word) = {
      if (word.nps) word.orth
      else word.orth + " "
    }

    val words = cclSentence.words.toArray
    val text = words.map(wordToWordWithSep _).mkString("").trim
    val chunks = cclSentence.chunks.groupBy(c => (c.channel, c.number)).map {
      case (chunkIdent, words) =>
        val head = words.toSeq.filter(_.head).map(_.index)
        Chunk(chunkIdent._2, chunkIdent._1, head.headOption.map(Index), words.map(x => Index(x.index)))
    } toArray

    Sentence(source, text, words, Map(), Map(), Some(chunks))
  }

  def readXML(input: InputStream) = {
    val xmlEvents = new XMLEventReader(Source.fromInputStream(input)(Codec.UTF8))

    val sentences = ArrayBuffer[CCLSentence]()
    var currentSentence: Option[CCLSentence] = None
    var currentWord: Option[Word] = None
    var currentStored: Option[String] = None
    var currentChunk: Option[CCLChunk] = None
    var ignoring = false
    var index = 0

    def addCurrentWord() = {
      for (word <- currentWord if word.ctag != "";
           sentence <- currentSentence) {
        sentence.words += word
      }

      currentWord = None
    }

    def storeText(unescapedText: String) = {
      val text = StringEscapeUtils.unescapeHtml4(unescapedText).replace("&apos;", "'")
      for (stored <- currentStored;
           word <- currentWord) {
        stored match {
          case "orth" =>
            currentWord = Some(word.copy(orth = word.orth + text))
          case "base" =>
            currentWord = Some(word.copy(base = word.base + text))
          case "ctag" =>
            currentWord = Some(word.copy(ctag = word.ctag + text))
        }
      }
    }


    for (event <- xmlEvents) {
      event match {
        case EvElemStart(_, "sentence", _, _) =>
          index = -1
          currentSentence = Some(CCLSentence(ArrayBuffer(), ArrayBuffer()))
        case EvElemEnd(_, "sentence") =>
          addCurrentWord()
          for (sentence <- currentSentence) {
            sentences += sentence
          }
        case EvElemStart(_, "tok", _, _) =>
          index += 1
          // New word! Firstly append last word if any!
          addCurrentWord()

          // Empty word!
          currentWord = Some(Word("", "", ""))

        case EvElemStart(_, "lex", attrs, _) =>
          ignoring = (attrs.get("disamb").flatMap(_.headOption).map(_.toString).getOrElse("0") != "1")
        case EvElemEnd(_, "lex") =>
          ignoring = false
        case EvElemStart(_, node, _, _) if STORE_NODES.contains(node) && !ignoring =>
          currentStored = Some(node)
        case EvElemEnd(_, node) if STORE_NODES.contains(node) =>
          currentStored = None
        case EvElemStart(_, "ns", _, _) =>
          // Word is added when new one starts! So we can modify currentWord here!
          for (word <- currentWord) {
            currentWord = Some(word.copy(nps = true))
          }

        case EvElemStart(_, "ann", attrs, _) =>
          val maybeChan = attrs.get("chan").flatMap(_.headOption)
          val head = (attrs.get("head").flatMap(_.headOption).map(_.toString()).getOrElse("0") == "1")
          for (chan <- maybeChan) {
            currentChunk = Some(CCLChunk(index, chan.toString(), 0, head))
          }
          currentStored = Some("chunk")
        case EvElemEnd(_, "ann") =>
          for (chunk <- currentChunk if chunk.number != 0;
               sentence <- currentSentence) {
            sentence.chunks += chunk
          }
          currentStored = None

        // text for chunk number
        case EvText(text) if currentStored == Some("chunk") =>
          for (chunk <- currentChunk) {
            currentChunk = Some(chunk.copy(number = text.toInt))
          }
          currentStored = None
        // Text for orth, base, ctag
        case EvText(text) if !ignoring =>
          storeText(text)
        case EvEntityRef(text) if !ignoring =>
          storeText(s"&$text;")
        case EvComment(text) if text.trim().startsWith("unknown entity") =>
          // dirty hack, as I couldn't find a way to introduce new entities! (&apos; is in the corpus)
          storeText(s"&${text.trim().substring(15)}")
        case _ => // ignore
      }
    }

    sentences
  }
}

object CCLSentenceReader {
  /// This implementation is too slow! Rewrite this using SAX or pull parser
  def read(source: String, input: InputStream, parseChunks: Boolean = true) = {
    val factory = javax.xml.parsers.SAXParserFactory.newInstance()

    // disable DTD validation
    factory.setValidating(false)
    factory.setFeature("http://xml.org/sax/features/validation", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)

    val root = scala.xml.XML.withSAXParser(factory.newSAXParser).load(input)
    // val root = XML.load(input)

    def wordToWordWithSep(word: Word) = {
      if (word.nps) word.orth
      else word.orth + " "
    }

    val sentences = (root \\ "sentence").map { sentNode =>
      val words = readSentence(sentNode).toIndexedSeq
      val text = words.map(wordToWordWithSep _).mkString("").trim
      val chunks = if (parseChunks) Some(readChunks(sentNode).toSeq) else None
      val sentence = Sentence(source, text, words, Map(), Map(), chunks)
      //if (words.isEmpty) {
      //  println(s"Error!!!!!!!!!!!!! Invalid sentence!!!!!!!!!! $sentNode ")
      //}
      sentence
    }

    sentences
  }

  def readSentence(node: Node) : Seq[Word] = {

    val nodes = node.child.filter(n => Set("tok", "ns").contains(n.label)).filter {
      // this filter removes toks that ae without lex (for some reasony pantera returned PiS - owi PiS-owi instead of only one segment)
      case node if node.label == "tok" => (node \\ "lex").exists(l =>(l \ "@disamb" text) == "1")
      case _ => true
    }.toSeq
    nodes.zipWithIndex.flatMap {
      case (token, index) if token.label == "tok" =>
        val orth = token \ "orth" text
        val lex = token \ "lex" \@ ("disamb", _ == "1")
        val base = lex \ "base" text
        val ctag = lex \ "ctag" text

        val nps = ((nodes.size > (index + 1)) && nodes(index + 1).label == "ns")

        Some(Word(orth, base, ctag, nps))
      case _ => None
    }
  }

  def readChunks(node: Node) = {
    (node \\ "tok").zipWithIndex.flatMap {
      case (token, index) => (token \\ "ann").flatMap {
        case ann =>
          val channel = ann \ "@chan" text
          val number = ann.text.toInt
          val isHead = (ann \ "@head").headOption.map(_.text.toInt).getOrElse(0) == 1
          if (number != 0) Some((index, channel, number, isHead))
          else None
      }
    }.groupBy(t => (t._3, t._2)).mapValues(seq => seq.map(x => (x._1, x._4))).map {
      case (chunkIdent, words) =>
        val head = words.filter(_._2).map(_._1)
        Chunk(chunkIdent._1, chunkIdent._2, head.headOption.map(Index), words.map(x => Index(x._1)))
    }
  }
}

object CCLReader {
  def read(input: InputStream) = {
    val root = XML.load(input)
    (root \\ "sentence").map(sentence => getId(sentence) -> readSentence(sentence)).toMap
  }

  def getId(sentence: Node) = sentence \ "@id" text

  def readSentence(node: Node) = {
    (node \\ "tok").zipWithIndex.flatMap {
      case (token, index) => (token \\ "ann").flatMap {
        case ann =>
          val channel = ann \ "@chan" text
          val number = ann.text.toInt
          val isHead = (ann \ "@head").headOption.map(_.text.toInt).getOrElse(0) == 1
          if (number != 0) Some((index, channel, number, isHead))
          else None
      }
    }.groupBy(t => (t._3, t._2)).mapValues(seq => seq.map(x => (x._1, x._4))).map {
      case (chunkIdent, words) =>
        val head = words.filter(_._2).map(_._1)
        Chunk(chunkIdent._1, chunkIdent._2, head.headOption.map(Index), words.map(x => Index(x._1)))
    }
  }
}