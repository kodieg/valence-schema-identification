package kodie.phd.tools.crf

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by kodie on 9/17/14.
 */
class TestCRF extends FlatSpec with Matchers {

  // This test really calls crf_learn and crf_tests not a mock, to make sure integration is working fine
  // (as there are no integration tests)
  "CRF" should "be able to build a model and use it" in {
    // given:
    val input = Array(
      Array("the\tthe", "the\tthe", "a\ta", "the\tthe"),
      Array("the\tthe", "a\ta")
    )
    val expected = List(
      "the\tthe\tthe", "the\tthe\tthe", "a\ta\ta", "the\tthe\tthe", "",
      "the\tthe\tthe", "a\ta\ta", ""
    )
    // when:
    val model = CRF.train("U01:%x[0,0]", input)
    val output = model.predict(input).toList

    // then:
    assert(output == expected)
  }
}
