package kodie.phd.formats

import org.scalatest.{DiagrammedAssertions, Matchers, FlatSpec}
import kodie.phd.skladnica.types.{Index, Chunk, Word}

/**
 * Created by kodie on 1/6/15.
 */
class TestCCLSentences extends FlatSpec with Matchers {
  "CCLSentenceReader" should "read sentences from inputStream" in {
    val input = getClass.getResourceAsStream("/ccl-sample.xml")
    val sentences = CCLSAXSentenceReader.read("test", input)

    assert(sentences.size === 2)

    val sentence = sentences(0)

    val text = "Dzięki inżynierii genetycznej opracowano szczepionkę na alergię pyłkową &gt; x. &apos;&apos;"
    val words = Seq(
       Word("Dzięki", "dzięki", "prep:dat"),
       Word("inżynierii", "inżynieria", "subst:sg:gen:f"),
       Word("genetycznej", "genetyczny", "adj:sg:gen:f:pos"),
       Word("opracowano", "opracować", "imps:perf"),
       Word("szczepionkę", "szczepionka", "subst:sg:acc:f"),
       Word("na", "na", "prep:acc"),
       Word("alergię", "alergia", "subst:sg:acc:f"),
       Word("pyłkową &gt; x", "pyłkową &gt; x", "adj:sg:acc:f:pos", nps = true), // had problems with &gt; parsing
       Word(".", ".", "interp"),
       Word("&apos;&apos;", "&apos;&apos;", "ign")
    )

    val chunks = Some(Seq(
      Chunk(1, "chunk_np", Some(Index(1)), Seq(0, 1, 2).map(Index.apply _)),
      Chunk(1, "chunk_vp", Some(Index(3)), Seq(3).map(Index.apply _)),
      Chunk(2, "chunk_np", Some(Index(4)), Seq(4).map(Index.apply _)),
      Chunk(3, "chunk_np", Some(Index(6)), Seq(5, 6, 7).map(Index.apply _))
    ).toSet)

    assert(sentence.text === text)
    assert(sentence.words === words)
    assert(sentence.chunks.map(_.toSet) === chunks)

  }
}
