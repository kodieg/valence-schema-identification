package kodie.phd.tools.wordnet

import org.scalatest.FlatSpec

/**
 * Created by kodie on 2/13/15.
 */
class TestPLWordnet extends FlatSpec {
  "PLWordnet" should "load wordnet from resources" in {
    val plWordnet = PLWordnet.cached()
    println(plWordnet.dictionary.get("kataklizm").map(_.toList))
    val lus = plWordnet.dictionary.get("kataklizm").get
    println(lus.map(plWordnet.lexicalUnitName).toList)
    val synsets = lus.flatMap(lu => plWordnet.luToSynsets.getOrElse(lu, Array())).toSet
    println(synsets.map(plWordnet.synsetName))
    val topSynsets = plWordnet.topSynsets("kataklizm")
    println(topSynsets.map(plWordnet.synsetName).mkString("; "))


  }
}
