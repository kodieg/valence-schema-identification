package kodie.phd.assigner

import org.scalatest.FlatSpec
import kodie.phd.skladnica.types.{Span, Word}


class TestSemanticHeadAssigner extends FlatSpec {
  "SemanticHeadAssigner" should "assign semantic head" in {
    val words = Seq(
       Word("Ona", "on", "ppron3:sg:nom:f:ter:akc:praep"),
       Word("poszła", "pójść", "praet:sg:f:perf"),
       Word("do", "do", "prep:gen"),
       Word("sklepu", "sklep", "subst:sg:gen:m3"),
       Word(",", ",", "interp"),
       Word("żeby", "żeby", "conj"),
       Word("kupić", "kupić", "inf:imperf"),
       Word("chleb", "chleb", "subst:sg:acc:m3"),
       Word(".", ".", "interp")
    )

    val args = Seq(
      Some((Span(1,2), "subj")),
      None,
      Some((Span(1,2), "prepnp(do,dop)")),
      None,
      None,
      Some((Span(1,2), "sentp(żeby)")),
      None,
      Some((Span(6,7), "np(bier)")),
      None
    )

    val semanticHeads = Seq(
      Some(0),
      None,
      Some(3),
      None,
      None,
      Some(6),
      None,
      Some(7),
      None
    )

    val result = SemanticHeadAssigner.assign(words, args)
    assert(result === semanticHeads)
  }

  "SemanticHeadAssigner" should "assign semantic head when prep label is invalid" in {
    val words = Seq(
      Word("Ona", "on", "ppron3:sg:nom:f:ter:akc:praep"),
      Word("poszła", "pójść", "praet:sg:f:perf"),
      Word("do", "do", "prep:gen"),
      Word("sklepu", "sklep", "subst:sg:gen:m3"),
      Word(",", ",", "interp"),
      Word("żeby", "żeby", "conj"),
      Word("kupić", "kupić", "inf:imperf"),
      Word("chleb", "chleb", "subst:sg:acc:m3"),
      Word(".", ".", "interp")
    )

    val args = Seq(
      Some((Span(1,2), "subj")),
      None,
      Some((Span(1,2), "prepnp(do,bier)")),  // invalid label here!
      None,
      None,
      Some((Span(1,2), "sentp(żeby)")),
      None,
      Some((Span(6,7), "np(bier)")),
      None
    )

    val semanticHeads = Seq(
      Some(0),
      None,
      Some(3),
      None,
      None,
      Some(6),
      None,
      Some(7),
      None
    )

    val result = SemanticHeadAssigner.assign(words, args)
    assert(result === semanticHeads)
  }
}
