package kodie.phd.skladnica

import kodie.phd.skladnica.types.{Argument, Span, Index}
import scala.xml.XML
import org.scalatest.FlatSpec
import scala.io.{Codec, Source}
import kodie.phd.skladnica.features._

/**
 * Created by kodie on 10/9/14.
 */
class TestFreePhrasesHandler extends FlatSpec {
  "SkladnicaTreeWalker" should "extract all arguments from the sentence" in {
    def testFile(file: String, result: Map[Span, List[Argument]]) {
      val input = XML.load(getClass.getResource(file))
      println(SkladnicaSentenceParser.findFreePhrases(input))
      println(SkladnicaSentenceParser.findArgumentStructure(input))
      val cont = Source.fromFile(getClass.getResource(file).toURI)(Codec.UTF8).getLines().mkString("")
      val sent= SkladnicaSentenceParser.parse("test", cont)
      println(extractBothArgumentWithPredicate(sent.get))

    }
    testFile("/tree-1.xml", Map(Span(2, 3) -> List(Argument(Span(0, 2), "np(narz)", Some(Index(1)), None),
      Argument(Span(3, 4), "np(bier)", Some(Index(3)), None))))
    /*testFile("/small-tree.xml", Map(Span(2, 3) -> List(Argument(Span(0, 2), "np(narz)", Some(Index(1)), None),
      Argument(Span(3, 4), "np(bier)", Some(Index(3)), None))))
    testFile("/small-tree-2.xml", Map(Span(2 ,3) -> List(Argument(Span(0, 2), "subj", Some(Index(0)), None),
      Argument(Span(3,5), "adjp(mian)", Some(Index(4)), None))))
    testFile("/small-tree-3.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))*/
  }
}
