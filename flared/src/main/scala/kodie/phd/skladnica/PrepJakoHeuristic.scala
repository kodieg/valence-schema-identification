package kodie.phd.skladnica

import kodie.phd.assigner.PredicateAssigner
import kodie.phd.skladnica.types.Word

/**
 * Created by kodie on 3/3/16.
 */
object PrepJakoHeuristic {
  def isPredicate(word: Word) = {
    PredicateAssigner.isPredicate(word)
  }

  def fillMissing(words: Seq[Word], labels: Array[Seq[(String, Double)]]) : Array[Seq[(String, Double)]] = {
    def isPrepAdjP(index: Int): Boolean = {
      var cur: Int = index + 1
      var hasAdj: Boolean = false
      while(cur < words.size && !isPredicate(words(cur)) && labels(index).headOption.map(_._1).getOrElse("_") != "_" && (cur - index) <= 5) {
        if (words(cur).ctag.startsWith("subst")) return false
        if (words(cur).ctag.startsWith("adj")) hasAdj = true
        cur += 1
      }

      val strCases = Set("subj", "np(mian)", "np(bier)", "np(dop)")
      if (cur < words.size && strCases(labels(cur).headOption.map(_._1).getOrElse("_")))
        return false
      // question is whether this heuristic is OK
      return hasAdj
    }

    labels.zipWithIndex.map { case (wordLabels, index) =>
      if (wordLabels.filter(x => x._1 != "_" && x._2 > 0.8).isEmpty && words(index).base == "jako") {
        if (isPrepAdjP(index)) {
          Seq("prepadjp(jako,str)" -> 1.0)
        } else {
          Seq("prepnp(jako,str)" -> 1.0)
        }
      } else {
        wordLabels
      }
    }
  }
}
