package kodie.phd.skladnica

import org.scalatest.{FlatSpec, Matchers}
import scala.xml.XML

import types._


/**
 * Created by kodie on 6/20/14.
 */
class TestSkladnicaSentenceParser extends FlatSpec with Matchers {

  "findSentenceText" should "extract sentence text " in {
    val sentenceText = "This is a sample sentence"
    val input = <forest><text>{sentenceText}</text></forest>

    SkladnicaSentenceParser.findSentenceText(input) should be (Some(sentenceText))
  }

  it should "return None when there is no sentece text" in {
    val input = <forest></forest>

    SkladnicaSentenceParser.findSentenceText(input) should be (None)
  }

  "findWords" should "extract all chosen words in the sentence" in {
    val input = <forest>
        <node nid="1" chosen="false">
          <terminal>
            <orth>Stolik</orth>
            <base>Stół</base>
            <f type="tag">subst:fg:nom</f>
          </terminal>
        </node>
        <node nid="2" chosen="true">
          <terminal nps="true">
            <orth>Krzesełko</orth>
            <base>krzesło</base>
            <f type="tag">subst:fg:nom</f>
          </terminal>
        </node>
        <node nid="3" chosen="true">
          <nonterminal>
          </nonterminal>
        </node>
        <node nid="4" chosen="true">
          <terminal>
            <orth>było</orth>
            <base>być</base>
            <f type="tag">praet:...</f>
          </terminal>
        </node>
    </forest>

    val expectedOutput = Seq(Word("Krzesełko", "krzesło", "subst:fg:nom", true), Word("było", "być", "praet:..."))

    SkladnicaSentenceParser.findWords(input) should be (expectedOutput)
  }

  "SkladnicaTreeWalker" should "extract all arguments from the sentence" in {
    def testFile(file: String, result: Map[Span, List[Argument]]) {
      val input = XML.load(getClass.getResource(file))
      //println(SkladnicaSentenceParser.findFreePhrases(input))
      SkladnicaSentenceParser.findArgumentStructure(input) should  equal (result)
    }



    /*testFile("/small-tree.xml", Map(Span(2, 3) -> List(Argument(Span(0, 2), "np(narz)", Some(Index(1)), None),
                                                     Argument(Span(3, 4), "np(bier)", Some(Index(3)), None))))
    testFile("/small-tree-2.xml", Map(Span(2 ,3) -> List(Argument(Span(0, 2), "subj", Some(Index(0)), None),
                                                       Argument(Span(3,5), "adjp(mian)", Some(Index(4)), None))))
    testFile("/small-tree-3.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))
    testFile("/tree-4.xml", Map(Span(5,6) -> List(Argument(Span(4,5),"subj",Some(Index(4)),None), Argument(Span(6,7),"np(cel)",Some(Index(6)),None)),
                                Span(12,13) -> List(Argument(Span(11,12),"np(bier)",Some(Index(11)),None)),
                                Span(0,1) -> List(testFile("/tree-infp.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))Argument(Span(1,2),"sie",Some(Index(1)),None), Argument(Span(2,9),"sentp(że)",Some(Index(3)),None), Argument(Span(10,13),"sentp(że)",Some(Index(10)),None)))

    )*/
    // testFile("/test-subj.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))
    //testFile("/tree-infp.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))

    // testFile("/tree-advp.xml", Map(Span(7, 8) -> List(Argument(Span(8, 10), "subj", Some(Index(8)), None))))
  }

  "findArgumentTypesForPredicates" should "find arguments in rekcja label" in {
    val input = <forest>
                  <node nid="0" from="4" to="6" chosen="true">
                   <nonterminal>
                     <category>ff</category>
                     <f type="wyróżnik">os</f>
                     <f type="aspekt">nd</f>
                     <f type="czas">prze</f>
                     <f type="tryb">ozn</f>
                     <f type="rodzaj">mnż</f>
                     <f type="liczba">poj</f>
                     <f type="osoba">3</f>
                     <f type="rekcja">[subj,np(mian),adjp(mian),prepnp(z,dop),advp]</f>
                     <f type="neg">tak</f>
                     <f type="dest">neut</f>
                     <f type="ink">ni</f>
                   </nonterminal>
                  <children rule="n_cz4">
                    <child nid="1" from="5" to="6" head="true"/>
                  </children>
                </node>
    </forest>

    val expectedArgumentTypes = Map(Span(4, 6) -> List("subj", "np(mian)", "adjp(mian)", "prepnp(z,dop)", "advp").sorted)

    SkladnicaSentenceParser.findArgumentTypesForPredicates(input) should equal (expectedArgumentTypes)
  }

  "findArgumentTypesForPredicates" should "find the arguments in test files" in {
    def testFile(file: String, result: Map[Span, List[String]]) {
      val input = XML.load(getClass.getResource(file))
      SkladnicaSentenceParser.findArgumentTypesForPredicates(input) should  equal (result)
    }

    testFile("/small-tree.xml", Map(Span(2, 3) -> List("np(narz)", "np(bier)").sorted))
    testFile("/small-tree-2.xml", Map(Span(2, 3) -> List("subj", "adjp(mian)").sorted))
    testFile("/small-tree-3.xml", Map(Span(7, 8) -> List("subj")))

  }

  "parseFreePhrase" should "recognize noun phrases" in {
    val input = <forest>
      <node nid="0" from="0" to="5" chosen="true">
        <nonterminal>
          <category>fl</category>
          <f type="wyróżnik">os</f>
          <f type="aspekt">nd</f>
          <f type="czas">prze</f>
          <f type="tryb">ozn</f>
          <f type="rodzaj">mnż</f>
          <f type="liczba">poj</f>
          <f type="osoba">3</f>
          <f type="rekcja">[subj,np(mian),adjp(mian),prepnp(z,dop),advp]</f>
          <f type="neg">tak</f>
          <f type="dest">neut</f>
          <f type="ink">ni</f>
        </nonterminal>
        <children rule="n_cz4" chosen="true">
          <child nid="1" from="0" to="5" head="true"/>
        </children>
      </node>
      <node nid="1" from="0" to="5" chosen="true">
      <nonterminal>
        <category>fno</category>
        <f type="przypadek">narz</f>
        <f type="rodzaj">żeń</f>
        <f type="liczba">poj</f>
        <f type="osoba">3</f>
        <f type="rekcja">[]</f>
        <f type="klasa">rzecz</f>
        <f type="zap">bzap</f>
        <f type="poz">pre</f>
        <f type="neg">tak</f>
        <f type="dest">neut</f>
        <f type="ink">ni</f>
      </nonterminal>
      <children rule="n_cz4">
        <child nid="1" from="5" to="6" head="true"/>
      </children>
    </node>
    </forest>

    SkladnicaSentenceParser.findFreePhrases(input) should be (List(Argument(Span(0, 5), "np(inst)", None, None)))
  }

  "parseFreePhrase" should "recognize prepositional phrases" in {
    val input = <forest>
      <node nid="0" from="0" to="5" chosen="true">
        <nonterminal>
          <category>fl</category>
          <f type="wyróżnik">os</f>
          <f type="aspekt">nd</f>
          <f type="czas">prze</f>
          <f type="tryb">ozn</f>
          <f type="rodzaj">mnż</f>
          <f type="liczba">poj</f>
          <f type="osoba">3</f>
          <f type="rekcja">[subj,np(mian),adjp(mian),prepnp(z,dop),advp]</f>
          <f type="neg">tak</f>
          <f type="dest">neut</f>
          <f type="ink">ni</f>
        </nonterminal>
        <children rule="n_cz4" chosen="true">
          <child nid="1" from="0" to="5" head="true"/>
        </children>
      </node>
      <node nid="1" from="0" to="5" chosen="true">
        <nonterminal>
          <category>fpm</category>
          <f type="przyim">o</f>
          <f type="przypadek">bier</f>
          <f type="klasa">rzecz</f>
          <f type="zap">bzap</f>
          <f type="neg">tak</f>
          <f type="dest">neut</f>
          <f type="ink">ni</f>
        </nonterminal>
        <children rule="n_cz4" chosen="true">
          <child nid="2" from="5" to="6" head="true"/>
        </children>
      </node>

      <node nid="2" chosen="true" from="5" to="6">
        <terminal>
        </terminal>
      </node>
    </forest>

    SkladnicaSentenceParser.findFreePhrases(input) should be (List(Argument(Span(0, 5), "prepnp(o,acc)", Some(Index(5)), None)))
  }
}
