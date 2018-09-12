package kodie.phd.skladnica

import scala.xml.NodeSeq

import types._

/**
 * Created by kodie on 6/28/14.
 */

object SkladnicaXMLImplicits {

  implicit class RichNodeSeq(ns: NodeSeq) {
    def \@(attribMatch: (String, String => Boolean)): NodeSeq = ns filter {
      _ \ ("@" + attribMatch._1) exists (s => attribMatch._2(s.text))
    }

    def id = ns \ "@nid" text

    def orth = ns \ "orth" text

    def base = ns \ "base" text

    def tag = ns \\ "f" \@("type", _ == "tag") text

    def predicateRection = ns \\ "f" \@("type", _ == "rekcja") text

    def category = ns \\ "category" text

    def fromIndex = Index((ns \ "@from" text).toInt)

    def toIndex = Index((ns \ "@to" text).toInt)

    def spanIndex = Span(fromIndex.i, toIndex.i)

    def isHead = (ns \ "@head" text) == "true"

    def isTerminal = !(ns \\ "terminal").isEmpty

    def chosenNodes = ns \\ "node" \@("chosen", _ == "true")

    def node(nid: String) = ns \\ "node" \@("nid", _ == nid)
  }

}

object SkladnicaConstants {
  val FINITIVE_PHRASE_CATEGORY = "ff"
  val REQUIRED_PHRASE_CATEGORY = "fw"
  val REQUIRED_PHRASE_TYPE_ATTRIBUTE = "tfw"
  val SENTENCE_CATEGORY = "zdanie"
  val FREE_PHRASE_CATEGORY = "fl"
  val SUBSENTENCE_CATEGORY = "fzd"


  val CASE = "przypadek"
  val PREPOSITION = "przyim"

  // TODO: think what to do with pop?
  val CASES = Map("mian" -> "nom", "dop" -> "gen", "cel" -> "dat", "bier" -> "acc", "narz" -> "inst",
                        "miej" -> "loc", "wol" -> "voc", "pop" -> "pop?", "str" -> "str")
  val REVCASES = CASES.map(a => (a._2 -> a._1)).toMap
}
