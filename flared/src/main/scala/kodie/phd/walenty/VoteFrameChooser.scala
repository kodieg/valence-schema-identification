package kodie.phd.walenty

import experimental.ArgumentWithProb
import kodie.phd.skladnica.types.Word

import scala.collection.mutable.ArrayBuffer

/**
 * Created by kodie on 10/13/16.
 */
class VoteFrameChooser(choosers: Seq[(FrameChooserCascade.Chooser, Double)], minVotes: Double) {

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], extendedSkladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]): (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    val res0 = choosers map { case (chooser, weight) =>
      chooser(predicateBase, aspect, words, skladnicaArguments, extendedSkladnicaArguments) -> weight
    }

    val res = res0 collect { case (r @ (Some(result), debug), weight) =>
      result.frame.frameText -> (weight, r)
    } groupBy (_._1)

    if (res.nonEmpty) {
      val top = res.mapValues(_.map(_._2._1).sum).maxBy(_._2)
      if (top._2 >= minVotes) {
        res(top._1).head._2._2
      } else {
        (None, res0.head._1._2)
      }
    } else {
      (None, res0.head._1._2)
    }
  }
}
