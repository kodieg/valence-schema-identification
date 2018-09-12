package kodie.phd.walenty

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Word

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class MaximalFrameChooserButIgnoringXP(walenty: NewWalentyParser, filterer: FrameFilterer = TrueFilterer, keepFull: Boolean=false) extends Serializable with MaxFrameProtocol {
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
            val ma = argumentsMatching.flatMap(_.map(_._2.text)).toSet
            maxFrameFull = frame.arguments.forall(s => ma(s.text))
            events += event.copy(info = "NEW_MAXIMAL_FRAME")
          } else if (matchedSkladnicaArguments.subsetOf(maxMatching) && matchedSkladnicaArguments != maxMatching) {
            events += event.copy(info = "SUBSET_OF_MAXIMAL")
            // subset!
          } else if (!maxMatching.isEmpty) {
            // so probably matchedSkladnicaArguments == maxMatching
            // currentMax frame is wrong, but maybe somewhere in the future we have a superset?

            val (wasXPAndNewHasPrep, wasPrepAndIsXP) = maxPairs.map { maxP =>
              val x = maxP.zip(argumentsMatching).map {
                case ((Some((maxArg, maxArgW)), Some((newArg, newArgW)))) =>
                  if (maxArgW.text == newArgW.text) { // TODO: maybe remove modifiers?
                    Some(0)
                  } else if(maxArgW.text.contains("xp(") && !newArgW.text.contains("xp(")) {
                    Some(-1)
                  } else if (newArgW.text.contains("xp(") && !maxArgW.text.contains("xp(")){
                    Some(1)
                  } else {
                    None
                  }
                case (None, None)  => Some(0)
                case _ => None
              }

              val wasXPAndNewHasPrep = x.forall(f => f.isDefined && f.get <= 0)
              val wasPrepAndIsXP = x.forall(f => f.isDefined && f.get >= 0)
              (wasXPAndNewHasPrep, wasPrepAndIsXP)
            }.getOrElse((false, false))

            if (wasXPAndNewHasPrep) {
              maxMatching = matchedSkladnicaArguments
              maxMatchingDebug = matchingArguments
              maxPairs = Some(argumentsMatching)
              events += event.copy(info = "NEW_MAXIMAL_FRAME")
            } else if (wasPrepAndIsXP) {
              events += event.copy(info = "PREP_PRECEDENCE_OVER_XP")
            } else {
              if (keepFull && maxFrameFull) {
                events += event.copy(info = "KEEPING_MAX_FRAME_FULL")
              } else {
                maxFrame = None
                events += event.copy(info = "NO_MAX_FRAME")
              }
            }
          } else {
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
