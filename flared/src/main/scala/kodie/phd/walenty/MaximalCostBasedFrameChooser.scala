package kodie.phd.walenty

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Word

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class MaximalCostBasedFrameChooser(walenty: NewWalentyParser, filterer: FrameFilterer = TrueFilterer) extends Serializable {

  def missingCost(arg: WalentyArgument): Double = {
    if (arg.modifiers.contains("obj")) return 1.0
    if (arg.realizations.exists(_.startsWith("cp("))) return 1.0
    if (arg.realizations.exists(_ == "or")) return 0.0
    if (arg.realizations.exists(_.startsWith("np("))) return 0.5
    return 0.5
  }

  def calcScore(frame: WalentyFrame, matching: Seq[Option[(ArgumentWithProb, WalentyArgument)]]) = {
    // TOOD: I could calculate for each frame probability that some argument occurs! and use this as scores!
    val found = matching.flatMap(_.map(_._2)).toSet
    val notFound = frame.arguments.filterNot(found)

    //
    found.size - notFound.map(missingCost).sum
  }

  def chooseMaximalFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    def chooseMaximalFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
      var maxFrame : Option[WalentyFrame] = None
      var maxMatching = Set[String]()
      var maxMatchingDebug = Seq[Option[WalentyArgument]]()
      var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None


      val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

      val results = for (frame <- frames) yield {
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
        if ((filterer.shouldUseFrame(frame, otherFormatArgumentsMatching))) {
          val score = calcScore(frame, argumentsMatching)
          // || matchingArguments.flatten.flatMap(_.realizations).exists(_.startsWith("lex"))) && !maxMatchingDebug.flatten.exists(_.realizations.exists(_.startsWith("lex")))) {
          Option(frame -> score)
        } else {
          None
        }
      }

//
//          // TODO: if frame has lexicalized arg and it was not matched that ignore whole frame (???)
//          // but it would be good to backoff to this frame if none was selected
//          // TODO: above idea may be good, but at the same time, it may be very bad
//          if (maxMatching.subsetOf(matchedSkladnicaArguments) && matchedSkladnicaArguments != maxMatching) {
//            frame -> score
//            maxFrame = Some(frame)
//            maxMatching = matchedSkladnicaArguments
//            maxMatchingDebug = matchingArguments
//            events += event.copy(info = "NEW_MAXIMAL_FRAME")
//          } else if (matchedSkladnicaArguments.subsetOf(maxMatching) && matchedSkladnicaArguments != maxMatching) {
//            events += event.copy(info = "SUBSET_OF_MAXIMAL")
//            // subset!
//          } else if (!maxMatching.isEmpty) {
//            // so probably matchedSkladnicaArguments == maxMatching
//            // currentMax frame is wrong, but maybe somewhere in the future we have a superset?
//            maxFrame = None
//            events += event.copy(info = "NO_MAX_FRAME")
//          } else {
//            events += event.copy(info = "INITIAL NO MATCHING")
//          }
//        } else {
//          events += event.copy(info = "FILTERED_OUT")
//        }
//      }

      val sortedFrames = results.sortBy(-_.map(_._2).getOrElse(-1.0))
      val maxValueOpt = sortedFrames.headOption.flatten.map(_._2)

      maxFrame = maxValueOpt match {
        case Some(maxValue) =>
          // Selection is ambiguous
          println(s"COST $maxValue --> ${sortedFrames.flatten.map(x => x._1.frameText -> x._2).toIndexedSeq} --> ${sortedFrames.flatten.filter(x => (math.abs(x._2 - maxValue) <= 0.5)).size > 1}")
          if (sortedFrames.flatten.filter(x => (math.abs(x._2 - maxValue) <= 0.5)).size > 1)
            None
          else
            sortedFrames.headOption.flatten.map(_._1)
        case None => None
      }

      val debugInfo = MaximalFrameChooserDebugInfo(events)
      maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size, Seq.empty)) -> debugInfo
    }

    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseMaximalFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}
