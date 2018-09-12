package kodie.phd.walenty

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Word
import phd.sr.data.WordsIndex
import phd.sr.scorer.pd.{VerbPseudoDisambiguationInstance, ScoreBasedVerbClassifier}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SRBaseChooser {
  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo)
}
/**
 * A bit simplistic approach (probably this is only sensible if classifiers contains exactly one classifier!)
 *
 * needs porting to the new interface!
 */
/*class SRValueFrameChooser(index: WordsIndex, walenty: NewWalentyParser, classifiers: Seq[ScoreBasedVerbClassifier]) extends SRBaseChooser with Serializable {

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[Option[Argument]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    var maxValue = 0.0
    var maxFrame: Option[WalentyFrame] = None
    var maxMatchingDebug = Seq[Option[WalentyArgument]]()


    val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

    def filterFrames(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[Option[Argument]]) = {
      val maybeFramesWithCounts = for (frame <- frames) yield {
        val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        val matchingArgsFlatten = matchingArguments.flatten

        /*println(frame)
        println(skladnicaArgumentsWithoutSie)
        println(matchingArguments)
*/
        val frameHasLex = frame.arguments.exists(_.realizations.exists(_.contains("lex")))
        val matchedHasLex = matchingArgsFlatten.exists(_.realizations.exists(_.contains("lex")))

        // TODO: I could use filterer! For some reason there are frames with only lex?
        if (frameHasLex && !matchedHasLex) {
          None
        } else {
          Some(matchingArgsFlatten.size -> frame)
        }
      }
      val framesWithCounts = maybeFramesWithCounts.flatten


      val maxCount = if (framesWithCounts.nonEmpty) framesWithCounts.map(_._1).max else -1

      val maxFrames = framesWithCounts.filter(_._1 == maxCount).map(_._2)

      maxFrames
    }


    def chooseFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[Option[Argument]]) = {
      val filFrames = filterFrames(frames, skladnicaArgumentsWithoutSie)

      for (frame <- filFrames) {
        val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        val argumentsMatching = skladnicaArgumentsWithoutSie.zip(matchingArguments)

        val (frameValue, debug) = computeFrameValue(frame.frameText, argumentsMatching)
        val event = MaximalFrameChooserDebugEvent(frame.frameText, "", argumentsMatching)

        if (frameValue > maxValue || filFrames.size == 1) {
          maxValue = frameValue
          maxFrame = Some(frame)
          maxMatchingDebug = matchingArguments
          events += event.copy(info = s"NEW_MAX_FRAME (value: $frameValue debug: $debug)")
        } else {
          events += event.copy(info = s"NOT_MAX_FRAME (value: $frameValue, max: $maxValue, debug: $debug)")
        }

      }


      val debugInfo = MaximalFrameChooserDebugInfo(events)
      maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size)) -> debugInfo
    }


    def computeFrameValue(frame: String, args: Seq[(Option[Argument], Option[WalentyArgument])]) = {
      val instanceTemplate = VerbPseudoDisambiguationInstance(-1, index.getIndexOrElse(frame, -1), -1, 0, 0, -1, 0)
      val value = args.view.collect {
        case (Some(argument), Some(walentyArgument)) if argument.semanticHead.isDefined =>
          val headBase = words(argument.semanticHead.get).base
          val headBaseIndex = index.getIndexOrElse(headBase, -1)
          val groupType = index.getIndexOrElse(walentyArgument.text, -1)
          val instance = instanceTemplate.copy(semanticHeadLemma = headBaseIndex, semanticHeadOrth = headBaseIndex, groupType = groupType)
          // Hm... maybe I should do pair to pair voting instead of counting some uncomparable scores? This seems
          classifiers.view.map { classifier =>
            classifier.synchronized {
              // a bit hacky use of score based classifier to get score!
              /*if (predicateBase.startsWith("iść") && frame.contains("xp(perl)")) {
                println(instance)
                println("group", instance.groupType -> index.getWord(instance.groupType))
                println("predicate", instance.predicate1 -> index.getWord(instance.predicate1), frame)
              }*/
              val (_, (one, _)) = classifier.pickWithDebug(instance)
              one._2
            }

          }.sum // is sum sensible? /// maybe math.log().sum would be better? it could be interpreted as P(frame)

      }.sum
      (value, s"val: $value")
    }

    val hasSieArgument = skladnicaArguments.exists(_.map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.filter(_.filter(_.argumentType != "sie").isDefined))
  }
}
*/
