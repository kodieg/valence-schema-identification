package kodie.phd.walenty

import kodie.phd.walenty.flat.{WalentySkladnicaBridge, FlatWalentyInstnce}
import org.scalatest.FlatSpec

/**
 * Created by kodie on 11/19/14.
 */
class TestWalenty extends FlatSpec {
  "WalentyInstance" should "load walenty db correctly" in {
    println(FlatWalentyInstnce.data.size)

    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("subj", Seq("subj", "sie", "np(str)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("sie", Seq("subj", "sie", "np(str)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("np(bier)", Seq("subj", "sie", "np(str)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("np(narz)", Seq("subj", "sie", "np(inst)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("prepnp(w,miej)", Seq("subj", "sie", "prepnp(w,loc)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("prepadjp(o,narz)", Seq("subj", "sie", "prepadjp(o,inst)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("sentp(że)", Seq("subj", "sie", "cp(że)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("sentp(pz)", Seq("subj", "sie", "cp(int)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("adjp(mian)", Seq("subj", "sie", "adjp(nom)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("infp(dk)", Seq("subj", "sie", "infp(perf)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("infp(nd)", Seq("subj", "sie", "infp(imperf)")))
    assert (WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("infp(dk)", Seq("subj", "sie", "infp(_)")))
    assert (!WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty("unknown(dk)", Seq("subj", "sie", "infp(_)")))

    assert (FlatWalentyInstnce.checkArgument("aresztować", "perf", "subj"))
    assert (FlatWalentyInstnce.checkArgument("aresztować", "imperf", "np(bier)"))
    assert (FlatWalentyInstnce.checkArgument("aresztować", "perf", "prepnp(za,bier)"))

    assert (FlatWalentyInstnce.checkArgument("awanturować", "imperf", "sie"))
    assert (FlatWalentyInstnce.checkArgument("awanturować", "imperf", "subj"))
    assert (FlatWalentyInstnce.checkArgument("awanturować", "imperf", "sentp(pz)"))
    assert (FlatWalentyInstnce.checkArgument("awanturować", "imperf", "sentp(że)"))
    assert (FlatWalentyInstnce.checkArgument("awanturować", "imperf", "prepnp(o,bier)"))

  }
}
