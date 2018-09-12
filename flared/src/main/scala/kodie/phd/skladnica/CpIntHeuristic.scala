package kodie.phd.skladnica

import kodie.phd.skladnica.types.Word
import kodie.phd.walenty.NCPMatcher

/**
 * Created by kodie on 2/10/16.
 */
object CpIntHeuristic {
  val QUESTION_WORDS = NCPMatcher.QUESTION_WORDS.toSet
  def isCpInt(words: Seq[Word], index: Int) = {
    val word = words(index)
    QUESTION_WORDS(word.base.toLowerCase) || QUESTION_WORDS(word.orth.toLowerCase)
  }

  def conjWord(word: Word) = {
    word.base ==   "i" || word.base == "również"
  }

  def fillMissing(words: Seq[Word], labels: Array[Seq[(String, Double)]]) : Array[Seq[(String, Double)]] = {
    labels.zipWithIndex.map { case (wordLabels, index) =>
      if (wordLabels.filter(x => x._1 != "_" && x._2 > 0.8).isEmpty && isCpInt(words, index) && !words.lift(index+1).exists(conjWord)) {
        Seq(("sentp(pz)" -> 9.0), (s"sentp(${words(index).base})" -> 0.9))
      } else {
        wordLabels
      }
    }
  }

}
