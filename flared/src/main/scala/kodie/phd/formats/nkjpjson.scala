package kodie.phd.formats

import java.io.InputStream
import scala.io.{Codec, Source}
import org.json.simple.JSONValue
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import scala.collection.JavaConverters._
import kodie.phd.skladnica.types.{Word, Sentence}
import kodie.phd.skladnica.SkladnicaConstants
import scala.util.Try

case class NKJP1MArgument(argType: String, syntacticHead: Int, semanticHead: Int)
// My custom format for SemanticHeadAssigner evaluation
// Generated by custom python code
object NKJP1MJson {
  def readSentences(input: InputStream) = {
     val sentences = for (
         line <- Source.fromInputStream(input)(Codec.UTF8).getLines();
         jsonSentence <- JSONValue.parse(line).asInstanceOf[JSONArray].iterator().asScala
       ) yield {
       Try(jsonToSentence(jsonSentence.asInstanceOf[JSONObject])).toOption
     }
     sentences.toList.flatMap(x => x).toSeq
  }

  def wordToWordWithSep(word: Word) = {
    if (word.nps) word.orth
    else word.orth + " "
  }

  def jsonToSentence(jsonSentence: JSONObject) : (Sentence, Seq[NKJP1MArgument]) = {
    val jsonWords = jsonSentence.get("words").asInstanceOf[JSONArray].iterator().asScala.asInstanceOf[Iterator[JSONObject]]
    val words = jsonWords.map(jsonToWord _).toIndexedSeq
    val text = words.map(wordToWordWithSep _).mkString("").trim
    val jsonGroups = jsonSentence.get("groups").asInstanceOf[JSONArray].iterator().asScala.asInstanceOf[Iterator[JSONObject]]
    val groups = jsonGroups.map(jsonToGroup(words)).filterNot(g => g.argType.startsWith("prepnp(temu")).toList
    val sent = Sentence("nkjp1M.json", text, words, Map(), Map(), None)

    //println(sent, groups)
    (sent, groups)
  }

  // Returns seq of option: Some(type, semHeadIndex)
  def argumentsToOptionSeq(sentence: Sentence, groups: Seq[NKJP1MArgument]) : Seq[Option[(String, Int)]] = {
    (0 until sentence.words.size) map {
      index =>
        groups.find(_.syntacticHead == index).map(arg => (arg.argType, arg.semanticHead))
    }
  }

  def jsonToGroup(words: Seq[Word])(jsonGroup: JSONObject) = {
    val nkjpGroupType = jsonGroup.get("group_type").toString
    val semHead = jsonGroup.get("sem_head").asInstanceOf[java.lang.Long].toInt
    val synHead = jsonGroup.get("syn_head").asInstanceOf[java.lang.Long].toInt
    val groupType = convertArgumentType(words, nkjpGroupType, synHead)

    NKJP1MArgument(groupType, synHead, semHead)
  }

  def wordToCase(ctag: String) = {
    val parts = ctag.split(":")
    val pos = parts(0)

    if (List("subst", "ger", "num", "psubst", "adj", "padj", "ppron12", "ppron3").contains(pos))
      parts(2)
    else if (List("siebie", "prep").contains(pos))
      parts(1)
    else
      "_"
  }
  def getCase(ctag: String) = {
    SkladnicaConstants.REVCASES(wordToCase(ctag))
  }

  def convertArgumentType(words: Seq[Word], nkjpGroupType: String, synHead: Int) = {
    nkjpGroupType match {
      case "NG" => s"np(${getCase(words(synHead).ctag)}\)"
      case "NumG" => s"np(${getCase(words(synHead).ctag)})" // ?
      case "AdjG" => s"adjp(${getCase(words(synHead).ctag)})"
      case "PrepNG" => s"prepnp(${words(synHead).base},${getCase(words(synHead).ctag)})"
      case "PrepNumG" => s"prepnp(${words(synHead).base},${getCase(words(synHead).ctag)})"
      case "PrepAdjP" => s"prepadjp(${words(synHead).base},${getCase(words(synHead).ctag)})"
      case "AdvG" => s"advp"
      case "CG" => s"sentp(${words(synHead).base})"
      case "KG" => s"sentp(int)"
      case "DisG" => s"disg"
    }
  }

  def jsonToWord(jsonWord: JSONObject) : Word = {
    val orth = jsonWord.get("orth").toString
    val interp = jsonWord.get("interp").asInstanceOf[JSONArray]
    val base = interp.get(0).toString
    val ctag = interp.get(1).toString
    val nps = jsonWord.get("nps").asInstanceOf[Boolean] == true

    Word(orth, base, ctag, nps)
  }

  def load() = {
    val corpusStream = getClass.getResourceAsStream("/nkjp1M.json")
    readSentences(corpusStream)
  }
}