package kodie.phd.walenty

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Word

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MaximalFrameChooserResult(frame: WalentyFrame, matchedArguments: Seq[Option[WalentyArgument]], candidatesCount: Int, argumentsMatching: Seq[Option[(ArgumentWithProb, WalentyArgument)]])

case class MaximalFrameChooserDebugEvent(frame: WalentyFrame, info: String, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])], frameSwapper: String = "", detailerDebug: List[String] = List.empty) {
  def frameString: String = frame.frameText
}
case class MaximalFrameChooserDebugInfo(events: Seq[MaximalFrameChooserDebugEvent])

trait FrameFilterer extends Serializable {
  def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]) : Boolean
}

object TrueFilterer extends FrameFilterer {
  override def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]): Boolean = true
}

object LexicalizedFilterer extends FrameFilterer {
  override def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]): Boolean = {
    def hasLexicalizedArgument(realizations: Seq[String]) = realizations.forall(_.startsWith("lex")) || realizations.forall(_.startsWith("comprep")) || realizations.forall(_.startsWith("fixed"))
    val frameWithLexArgument = frame.arguments.exists(arg => hasLexicalizedArgument(arg.realizations))
    val matchingHasLexArgument = matchedElements.exists {
      case (Some(_), Some(walentyArgument)) => hasLexicalizedArgument(walentyArgument.realizations)
      case _ => false
    }

   // println(s"$frameWithLexArgument --- $matchingHasLexArgument")

    // If frame has lex argument, some lex argument must be matched!
    (!frameWithLexArgument) || matchingHasLexArgument
  }
}

object InfFilterer extends FrameFilterer {
  override def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]): Boolean = {
    // realizations.exists(...) yields very similar results
    def hasInfinitiveArgument(realizations: Seq[String]) = realizations.forall(_.startsWith("infp"))
    val frameWithInfArgument = frame.arguments.exists(arg => hasInfinitiveArgument(arg.realizations))
    val matchingHasInfArgument = matchedElements.exists {
      case (Some(_), Some(walentyArgument)) => hasInfinitiveArgument(walentyArgument.realizations)
      case _ => false
    }

    // If frame has inf argument, some inf argument must be matched!
    (!frameWithInfArgument) || matchingHasInfArgument
  }
}

class AndFilterer(f1: FrameFilterer, f2: FrameFilterer) extends FrameFilterer {
  override def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]): Boolean = {
    f1.shouldUseFrame(frame, matchedElements) && f2.shouldUseFrame(frame, matchedElements)
  }
}

class MinArgsMatchedFilterer(minCount: Int) extends FrameFilterer {
  override def shouldUseFrame(frame: WalentyFrame, matchedElements: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]): Boolean = {
    matchedElements.filter(x => x._1.isDefined && x._2.isDefined).size >= minCount
  }
}

trait  MaxFrameProtocol {
  def chooseMaximalFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo)
  }

class MaximalFrameChooser(walenty: NewWalentyParser, filterer: FrameFilterer = TrueFilterer, keepFull: Boolean=false) extends Serializable  with MaxFrameProtocol {
  def chooseMaximalFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    def chooseMaximalFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
      var maxFrame : Option[WalentyFrame] = None
      var maxMatching = Set[String]()
      var maxMatchingDebug = Seq[Option[WalentyArgument]]()
      var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None
      var maxFrameFull = false

      val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

      for (frame <- frames) {
        // TODO: if lex frame matched it's more valueable than anything else! Well... tests has shown that that's not true (why?)
        val argumentsMatching = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        val matchingArguments = argumentsMatching.map {
          case Some((_, walenty)) => Some(walenty)
          case None => None
        }
        //val argumentsMatching = skladnicaArgumentsWithoutSie.zip(matchingArguments)
        val matchedSkladnicaArguments = argumentsMatching.collect {
          case Some((skladnicaArgument, _)) => skladnicaArgument.argumentType
        }.toSet

        val otherFormatArgumentsMatching = argumentsMatching.map {
          case Some((arg, walentyArg)) => (Some(arg), Some(walentyArg))
          case None => (None, None)
        }

        val event = MaximalFrameChooserDebugEvent(frame, "", otherFormatArgumentsMatching)

        // choosing always when lex was matched yields worse results!
        // but seem to work better on 300M corpus :)
        if ((filterer.shouldUseFrame(frame, otherFormatArgumentsMatching))) { // || matchingArguments.flatten.flatMap(_.realizations).exists(_.startsWith("lex"))) && !maxMatchingDebug.flatten.exists(_.realizations.exists(_.startsWith("lex")))) {

          // TODO: if frame has lexicalized arg and it was not matched that ignore whole frame (???)
          // but it would be good to backoff to this frame if none was selected
          // TODO: above idea may be good, but at the same time, it may be very bad

          if (maxMatching.subsetOf(matchedSkladnicaArguments) && matchedSkladnicaArguments != maxMatching) {
            maxFrame = Some(frame)
            maxMatching = matchedSkladnicaArguments
            maxMatchingDebug = matchingArguments
            maxPairs = Some(argumentsMatching)
            events += event.copy(info = "NEW_MAXIMAL_FRAME")
          } else if (matchedSkladnicaArguments.subsetOf(maxMatching) && matchedSkladnicaArguments != maxMatching) {
            events += event.copy(info = "SUBSET_OF_MAXIMAL")
            // subset!
          } else if (!maxMatching.isEmpty) {
            // so probably matchedSkladnicaArguments == maxMatching
            // currentMax frame is wrong, but maybe somewhere in the future we have a superset?
            if (keepFull && maxFrameFull) {
              events += event.copy(info = "KEEPING_MAX_FRAME_FULL")
            } else {
              maxFrame = None
              events += event.copy(info = "NO_MAX_FRAME")
            }
          }   else {
            events += event.copy(info = "INITIAL NO MATCHING")
          }
        } else {
          events += event.copy(info = "FILTERED_OUT")
        }
      }

      val debugInfo = MaximalFrameChooserDebugInfo(events)
      maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size, maxPairs.get)) -> debugInfo
    }

    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseMaximalFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}

class MultiFrameChooser(walenty: NewWalentyParser, filterer: FrameFilterer = TrueFilterer) extends Serializable {
  def chooseMatchingFrames(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : Seq[(Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo)] = {

    def chooseMatchingFramesFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
      var matchingFrames = new ArrayBuffer[(WalentyFrame, Seq[Option[WalentyArgument]], Seq[Option[(ArgumentWithProb, WalentyArgument)]])]()

      val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

      for (frame <- frames) {
        // TODO: if lex frame matched it's more valueable than anything else! Well... tests has shown that that's not true (why?)
        val argumentsMatching = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        val matchingArguments = argumentsMatching.map {
          case Some((_, walenty)) => Some(walenty)
          case None => None
        }
        //val argumentsMatching = skladnicaArgumentsWithoutSie.zip(matchingArguments)
        val matchedSkladnicaArguments = argumentsMatching.collect {
          case Some((skladnicaArgument, _)) => skladnicaArgument.argumentType
        }.toSet

        val otherFormatArgumentsMatching = argumentsMatching.map {
          case Some((arg, walentyArg)) => (Some(arg), Some(walentyArg))
          case None => (None, None)
        }
        val event = MaximalFrameChooserDebugEvent(frame, "", otherFormatArgumentsMatching)

        // TODO: add debug info?
        if ((filterer.shouldUseFrame(frame, otherFormatArgumentsMatching))) {
          matchingFrames += ((frame, matchingArguments, argumentsMatching))
        }
      }

      val debugInfo = MaximalFrameChooserDebugInfo(events)
      matchingFrames.map(frame => Option(MaximalFrameChooserResult(frame._1, frame._2, frames.size, frame._3)) -> debugInfo)

    }

    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseMatchingFramesFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }

}