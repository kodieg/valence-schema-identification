package kodie.phd.skladnica

import kodie.phd.skladnica.types.Word



object NonchHeuristic {
  val NONCH_BASES = Set("coś", "nic", "co", "sporo", "wiele", "dużo", "mało", "niewiele", "trochę")


  def fillMissing(words: Seq[Word], labels: Array[Seq[(String, Double)]]) : Array[Seq[(String, Double)]] = {
    def nextWord(index: Int) = words.lift(index + 1)
    labels.zipWithIndex.map { case (wordLabels, index) =>
      if (wordLabels.filter(x => x._1 != "_" && x._2 > 0.8).isEmpty) {
        val word = words(index)
        if (NONCH_BASES(word.base) || (word.base == "to" && nextWord(index).map(_.base).getOrElse("") == "samo")) {
          Seq("nonch" -> 1.0) ++ wordLabels
        } else {
          wordLabels
        }
      } else {
        wordLabels
      }
      //TODO: skonczyc nonch i dodac to i siebie do MultiLabelSparkEHelpr!
    }
  }
}
