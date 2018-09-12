package kodie.phd.skladnica

import kodie.phd.skladnica.types.Word

/**
 * Created by kodie on 3/3/16.
 */
object SiebieHeuristic {
  def fillMissing(words: Seq[Word], labels: Array[Seq[(String, Double)]]) : Array[Seq[(String, Double)]] = {
    labels.zipWithIndex.map { case (wordLabels, index) =>
      if (words(index).orth.toLowerCase == "siebie") Seq("refl" -> 1.0) ++ wordLabels
      else wordLabels
    }
  }
}
