package kodie.phd.walenty

import phd.sr.counts.BaseCounts
import phd.sr.data.WordsIndex

import scala.util.Random

/**
 * Created by kodie on 10/6/15.
 */
class FrameSwapperBuilder(walenty: NewWalentyParser, index: WordsIndex, predicateCounts: collection.Map[Int, Float]) extends Serializable {
  val frameTexts = walenty.frames.values.flatten.map(_.frameText)
  val byFrame = frameTexts.groupBy(getFrame _).mapValues {
    _.map(getOnlyPredicate _).toSet
  }.map(identity _)

  val byFrameFullPredicate = frameTexts.groupBy( x => getFrame(x) -> getOnlyPredicate(x)).mapValues(_.toSeq)


  def buildSwapper(availableFrames: Seq[WalentyFrame], predicate: Option[String]) : Seq[String => String] = {
    val availableFrameTexts = availableFrames.map(_.frameText)
    val availableDefs = availableFrameTexts.map(getFrame _)
    val possiblePredicates = availableDefs.map(byFrame.getOrElse(_, Set.empty)).reduceLeft(_ intersect _).filter(_ != predicate.getOrElse("___"))

    val possibleSwappers = possiblePredicates.toSeq.map { predicate =>
      val swapper = new FrameSwapper(predicate, byFrameFullPredicate)
      val swapperFrameScores = availableFrameTexts.map(swapper).map { predWithFrame =>
        val wordIndex = index.getIndexOrElse(predWithFrame, -1)
        val count: Float = if (wordIndex == -1) 0.0f else predicateCounts.getOrElse(wordIndex, 0.0f)
        count
      }
      val score = /*swapperFrameScores.min */ swapperFrameScores.sum /  swapperFrameScores.size
      score -> swapper
    }

    if (possibleSwappers.isEmpty) {
      //if (Random.nextInt(100) < 33) println(s"Could not find swapper fn for ${availableFrames.map(_.frameText).mkString(", ")}")
      Seq(identity)
    } else {
      //if (Random.nextInt(100) < 33) println(s"Got swapper ${possibleSwappers.maxBy(_._1)._2.predicateTo} (of ${possibleSwappers.size}) for ${availableFrames.map(_.frameText).mkString(", ")}")
      possibleSwappers.toSeq.sortBy(-_._1).map(_._2)
    }
  }

  def getFrame(frameText: String) = FrameSwapperUtils.splitFrame(frameText)._2
  def getPredicate(frameText: String) = FrameSwapperUtils.splitFrame(frameText)._1
  def getOnlyPredicate(frameText: String) = frameText.split(":")(0).trim
}

case class FrameSwapper(predicateTo: String, frames: Map[(String, String), Seq[String]]) extends (String => String) {
  override def apply(originalFrame: String): String = {
    val (_, frame) = FrameSwapperUtils.splitFrame(originalFrame)
    val onlyPred = getOnlyPredicate(originalFrame)
    frames.get((frame, predicateTo)).map(_.head).getOrElse(originalFrame)
  }

  def getOnlyPredicate(frameText: String) = frameText.split(":")(0).trim

}

object FrameSwapperUtils {
  def splitFrame(fullFrame: String) : (String,String) = {
    val fourthColonIndex = nthCharIndex(fullFrame, ':', 5)
    if (fourthColonIndex == -1) {
      println(s"WTF??? $fullFrame")
    }
    val predicate = fullFrame.substring(0, fourthColonIndex)
    val rest = fullFrame.substring(fourthColonIndex + 1)
    (predicate, rest)
  }

  def nthCharIndex(fullFrame: String, expCh: Char, n: Int) : Int = {
    var nn = n
    for ((ch, i) <- fullFrame.zipWithIndex) {
      if (ch == expCh) nn -= 1
      if (nn == 0) return i
    }
    return -1
  }

  def joinPredicateAndFrame(pred: String, frame: String) = s"$pred:$frame"
}