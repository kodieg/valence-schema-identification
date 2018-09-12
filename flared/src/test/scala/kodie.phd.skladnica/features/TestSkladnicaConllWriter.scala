package kodie.phd.skladnica.features

import org.scalatest.{Matchers, FlatSpec}

import kodie.phd.skladnica.types._

/**
 * Created by kodie on 9/14/14.
 */
class TestSkladnicaConllWriter extends FlatSpec with Matchers {
  "SkladnicaConllWriter" should "output a sentence in CoNLL format" in {
    // given:
    val featureExtractors = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _) map (Function.tupled(_))
    val writer = new SkladnicaConllWriter(featureExtractors, extractArgumentTypeLabels)

    val words = Seq(("This", "this", "subst:sg:nom"), ("is", "be", "fin"), ("a", "a", "det"),
                    ("sample", "sample", "adj:sg:acc"), ("sentence", "sentence", "subst:sg:acc"),
                    ("yesterday", "yesterday", "time")).map(w => Word(w._1, w._2, w._3))
    val thisArgument = Argument(Span(0,1), "subj", Some(Index(0)), None)
    val sentenceArgument = Argument(Span(2,5), "np(acc)", Some(Index(4)), None)

    val sentence = Sentence("test.txt", "This is a sample sentence yesterday", words,
                            Map(Span(1,2) -> Seq(thisArgument, sentenceArgument)),
                            Map(Span(1,2) -> Seq(Argument(Span(5,6), "adjunct", Some(Index(5)), None))))

    // when:
    val conllOutput = writer.extract(sentence)

    // then:
    val expectedOutput = Array(
      "This\tthis\tsubst\tnom\tsubj",
      "is\tbe\tfin\t_\t_",
      "a\ta\tdet\t_\t_",
      "sample\tsample\tadj\tacc\t_",
      "sentence\tsentence\tsubst\tacc\tnp(acc)",
      "yesterday\tyesterday\ttime\t_\t_"
    )
    assert(conllOutput.toList == expectedOutput.toList)
  }
}
