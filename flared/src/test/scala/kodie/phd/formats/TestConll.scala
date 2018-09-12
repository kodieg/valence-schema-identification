package kodie.phd.formats

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by kodie on 9/14/14.
 */
class TestConll extends FlatSpec with Matchers {
  "ConllWriter" should "extract features and format them in Conll format" in {
    // given:
    val features = List((s: String) => s.substring(0, 1), (s: String) => s.substring(1, 2))
    val output = ((s: String) => s.size.toString)
    val writer = new ConllWriter[String](features, output)

    // when:
    val conllLine : String = writer.extract("Example")

    // then
    assert(conllLine == "E\tx\t7")
  }

  "ConllReader" should "split predicted streams into sentences" in {
    // given:
    val classifierOutput = "a\tc1\nb\tc2\n\nc\tc1\nd\tc2\n"
    val expectedOutput = Array(Array("c1", "c2"), Array("c1", "c2"))

    // when:
    val result = ConllReader.extractPredictedClasses(classifierOutput.split("\n").toStream)

    // then:
    assert(result === expectedOutput)
  }
}
