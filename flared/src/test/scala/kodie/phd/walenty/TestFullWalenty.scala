package kodie.phd.walenty

import kodie.phd.walenty.old._
import org.scalatest.FlatSpec

/**
 * Created by kodie on 2/3/15.
 */
class TestFullWalenty extends FlatSpec {
   "parseArgument" should "parse argument with all realizations and labels" in {
     assert(WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)")) === WalentyParser.parseArgument("subj{np(str)}") )
     assert(WalentyArgument(IndexedSeq(), IndexedSeq("np(str)")) === WalentyParser.parseArgument("{np(str)}"))
     assert(WalentyParser.parseArgument("{np(str); prepnp(na,acc)}") === WalentyArgument(IndexedSeq(), IndexedSeq("np(str)", "prepnp(na,acc)")))
     assert(WalentyParser.parseArgument("controllee,controller2{infp(_)}") === WalentyArgument(IndexedSeq("controllee","controller2"), IndexedSeq("infp(_)")))
     assert(WalentyParser.parseArgument("obj,controller{np(str); ncp(str,int); ncp(str,że)}") === WalentyArgument(IndexedSeq("obj", "controller"), IndexedSeq("np(str)", "ncp(str,int)", "ncp(str,że)")))
   }

  "parseArguments" should "parse full arguments specs" in {
    val input = "subj{np(str)} + obj,controller{np(str); ncp(str,int); ncp(str,że)} + controllee{prepnp(za,acc)}"
    val goldArguments = IndexedSeq(
      WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)")),
      WalentyArgument(IndexedSeq("obj", "controller"), IndexedSeq("np(str)", "ncp(str,int)", "ncp(str,że)")),
      WalentyArgument(IndexedSeq("controllee"), IndexedSeq("prepnp(za,acc)"))
    )
    assert(WalentyParser.parseArguments(input) === goldArguments)
  }

  "parseLine" should "parse a full line of Walenty" in {
    val input = "uznawać: imperf: subj{np(str)} + obj,controller{np(str); ncp(str,int); ncp(str,że)} + controllee{prepnp(za,acc)}\n"
    val goldArguments = IndexedSeq(
      WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)")),
      WalentyArgument(IndexedSeq("obj", "controller"), IndexedSeq("np(str)", "ncp(str,int)", "ncp(str,że)")),
      WalentyArgument(IndexedSeq("controllee"), IndexedSeq("prepnp(za,acc)"))
    )
    assert(WalentyParser.parseLine(input) === Some(("uznawać", "imperf", goldArguments)))
  }

  "matchingElements" should "match simple matching arguments" in {
    val skladnicaArgs = Seq("prepnp(za,miej)", "subj", "np(bier)", "prepnp(z,narz)")
    val walentyArgs = Seq(WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)")),
                          WalentyArgument(IndexedSeq("obj"), IndexedSeq("np(str)")),
                          WalentyArgument(IndexedSeq(), IndexedSeq("prepnp(za,loc)", "np(loc)")))
   // val pairs = SimpleWalentyFrameMatcher.matchingElements(skladnicaArgs, walentyArgs)
    // println(pairs.flatten)
   //  assert(pairs.flatten.size == 3)
  }


  "matchingElements" should "match simple matching arguments including xp" in {
    val skladnicaArgs = Seq("prepnp(za,miej)", "subj", "np(bier)", "prepnp(z,narz)", "prepnp(o,miej)")
    val walentyArgs = Seq(WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)")),
      WalentyArgument(IndexedSeq("obj"), IndexedSeq("np(str)")),
      WalentyArgument(IndexedSeq(), IndexedSeq("prepnp(za,loc)", "np(loc)")),
      WalentyArgument(IndexedSeq(), IndexedSeq("xp(temp)")))
    //val pairs = SimpleWalentyFrameMatcher.matchingElements(skladnicaArgs, walentyArgs)
    //println(pairs.flatten)
    //assert(pairs.flatten.size == 4)
  }


  "findMaximalFrame" should "find a maximal frame if it exists" in {
    val walenty = new NewWalenty()
    val pred = "aktywizować"
    val aspect = "imperf"
    val skladnicaArgs0 = Seq("subj", "sentp(żeby)")
    val skladnicaArgs1 = Seq("np(narz)")
    val skladnicaArgs2 = Seq("subj")
    val skladnicaArgs3 = Seq("subj", "sentp(żeby)", "np(narz)")

    /*assert(walenty.findMaximalFrame(pred, aspect, skladnicaArgs0) != None)
    assert(walenty.findMaximalFrame(pred, aspect, skladnicaArgs1) != None)
    assert(walenty.findMaximalFrame(pred, aspect, skladnicaArgs2) == None)
    assert(walenty.findMaximalFrame(pred, aspect, skladnicaArgs3) == None)*/

  }

  "WalentyRelizationsList" should "load source file" in {
    val realizations = new WalentyRelizationsList()
    val advp = realizations.realizations("advp(abl)")
    assert(advp.contains("skąd"))
    assert(advp.contains("stąd"))
    assert(advp.contains("znikąd"))

    val xp = realizations.realizations("xp(abl)")
    assert(xp.contains("cp(skąd)"))
    assert(xp.contains("prepnp(sponad,gen)"))


    val skad = realizations.skladnicaPossibilities("skąd")
    assert(skad === Seq("xp(abl)", "advp(abl)"))

    // not fixed here! need to use NewWalentySkladnicaBrdige.translate
    //val zz = realizations.skladnicaPossibilities("prepnp(o,miej)")
   // println(zz)
  }

  "NewWalentySkladnicaBridge" should "be able to match xp args" in {
    val realizations = new WalentyRelizationsList()
    val walentyArg = WalentyArgument(IndexedSeq("subj"), IndexedSeq("np(str)", "xp(temp)"))
    assert(NewWalentySkladnicaBridge.realizesXPArgument(realizations, "prepnp(o,loc)", walentyArg))
  }
}
