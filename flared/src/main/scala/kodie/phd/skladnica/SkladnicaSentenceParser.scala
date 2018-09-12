package kodie.phd.skladnica

import kodie.phd.skladnica.phrases.{FreePhraseStructureHandler, SentencePhraseHandler, FreePhraseHandler}
import walker.{Category, NodeHandler, SkladnicaTreeWalker, TerminalHeadLocator}
import scala.xml.{XML, NodeSeq}


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import types._
import SkladnicaXMLImplicits._


package object types {

  case class Index(i: Int) extends AnyRef {}

  case class Span(left: Int, right: Int)

  case class Word(orth: String, base: String, ctag: String, nps: Boolean = false)

  case class Argument(span: Span, argumentType: String, syntacticHead: Option[Index], semanticHead: Option[Index])

  case class Sentence(source: String, text: String, words: Seq[Word], argumentStructures: Map[Span, Seq[Argument]],
                      freePhrases: Map[Span, Seq[Argument]], chunks: Option[Seq[Chunk]] = None) {
  }

  case class Chunk(number: Int, chunkType: String, head: Option[Index], words: Seq[Index])
}

/**
 * Parser for Skladnica sentences.
 *
 * Created by kodie on 6/28/14.
 */
object SkladnicaSentenceParser {

  /**
   * Parses the content and extract argument structure and free phrases (adjuncts)
   * @param fileName name of the source file (used only for tracing the source of the sentcence)
   * @param content content of the source sentence file
   * @ returns Some(sentence) if sentence is fully parsed, None otherwise
   *
   */
  def parse(fileName: String, content: String) = {
    val root = XML.loadString(content)
    if (getAnswerType(root) == "FULL") {
      // Possibly sentp fixer lowers performance of a model?
      Some(
      // THIS IS HACKY SOLUTION TO FIX SENTP(PZ) syntactic head bug
        /*SentPFixer.fixSentP(*/Sentence(
          fileName,
          findSentenceText(root).getOrElse("-- missing text --"),
          findWords(root),
          findArgumentStructure(root),
          findFreePhrases(root)
        )//)
      )
    } else {
      None
    }
  }

  private[this] def getAnswerType(root: NodeSeq) = root \\ "base-answer" \ "@type" text

  /**
   * Finds the text of the sentence
   * @param root XML parsed sentence file
   * @return sentece text
   */
  def findSentenceText(root: NodeSeq) = (root \ "text").headOption.map(_.text)

  /**
   * Finds list of words
   * @param root XML parsed sentence file
   * @return List of Word
   */
  def findWords(root: NodeSeq) = {
    def terminalToWord(node: NodeSeq) = {
      (Word(node.orth,
        node.base,
        node.tag,
        (node \\ "@nps" text) == "true"))
    }
    val terminals = root.chosenNodes.map(x => (x.fromIndex, x)).flatMap(x => x._2 \\ "terminal" map (y => (x._1, y)))
    terminals.map(x => (x._1, terminalToWord(x._2))).sortBy(_._1.i).map(_._2).toArray
  }

  /**
   * Finds the list of free phrases (adjuncts)
   * @param root XML parsed sentence file
   * @return List of Argument objects (adjuncts)
   */
  def findFreePhrases(root: NodeSeq) = {
    //def parseFreePhrase(root: NodeSeq, freePhrase: NodeSeq) = {
      /*var syntacticHead: Option[Index] = None
      val freePhraseHandler = new FreePhraseHandler
      val syntacticHeadFinder = new TerminalHeadLocator {
        override def update(head: NodeSeq) = { syntacticHead = Some(head.fromIndex) }
      }

      SkladnicaTreeWalker.walk(root, freePhrase, List(freePhraseHandler, syntacticHeadFinder))
      val argumentType = freePhraseHandler.buildType
      Argument(freePhrase.spanIndex, argumentType, syntacticHead, None)
    }*/
    val clauseHandler = new FreePhraseStructureHandler
    SkladnicaTreeWalker.start(root, List(clauseHandler))
    clauseHandler.buildStructures()
    /*
    val freePhraseNodes = root.chosenNodes.filter(_.category == SkladnicaConstants.FREE_PHRASE_CATEGORY)
    freePhraseNodes.map(parseFreePhrase(root, _))*/
  }


  /**
   * Finds the predicate-argument structure (a mapping from predicate to sequence of arguments)
   * @param root XML parsed sentence file
   * @return map predicate -> arguments sequence
   */
  def findArgumentStructure(root: NodeSeq) = {
    // TODO: sentp(pz) is always wrong! I should fix parser...
    val clauseHandler = new SentencePhraseHandler
    SkladnicaTreeWalker.start(root, List(clauseHandler))
    clauseHandler.buildStructures()
  }

  /*
   * Reads argument types from "rekcja" tag.
   *
   * Used for validation of findArgumentStructure method.
   */
  def findArgumentTypesForPredicates(root: NodeSeq) = {
    def parseFrame(rekcja: String) = {
      require((rekcja.head == '[') && (rekcja.last == ']'))
      // Magic regex that matches frame elements (i.e. plain words - as advp,
      // or words with arguments - prepnp(do,bier))
      val cleanedRekcja = rekcja.substring(1, rekcja.length - 1)
      val pattern = raw"[^,\(]*(?:\([^\)]*\))?".r
      val frames = pattern.findAllIn(cleanedRekcja).filterNot(_.isEmpty).map(_.trim).toList.sorted
      frames
    }
    val finitivePhrases = root.chosenNodes.filter(_.category == SkladnicaConstants.FINITIVE_PHRASE_CATEGORY)
    val predicatesWithRectionTag = finitivePhrases.iterator.map { p => p.spanIndex -> p.predicateRection}.toMap
    predicatesWithRectionTag.mapValues(parseFrame).toMap
  }
}