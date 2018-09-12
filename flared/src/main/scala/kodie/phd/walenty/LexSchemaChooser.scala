package kodie.phd.walenty

import experimental.ArgumentWithProb
import kodie.phd.skladnica.types.Word

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by kodie on 9/8/15.
 */
class LexSchemaChooser(walenty: NewWalentyParser) extends SRBaseChooser with Serializable {


  def scoreFrame(words: Seq[Word], frame: WalentyFrame, matchedArgs: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])], otherFrame: WalentyFrame, otherMatchedArgs: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])]) : (Int, List[(String, String, String, String, String, String, String)]) = {
    val size = matchedArgs.size

    val allArgs = matchedArgs.zip(otherMatchedArgs.map(_._2))

    def hasLexArgument(arg: WalentyArgument) = arg.realizations.exists(_.startsWith("lex")) || arg.realizations.exists(_.startsWith("comprep")) ||  arg.realizations.exists(_.startsWith("fixed"))

    def dprintln(s: String) = (); //if (math.abs(thr - 0.1)   < 0.000000001) println(s)

    var betterArg = 0
    var i = 0

    var debugValuesInfo: List[(String, String, String, String, String, String, String)] = Nil
    // TODO: this can be simplified if otherFrame == frame always!
    for (triple <- allArgs) {

      //println(triple)
      betterArg += (triple match {
        case ((None, _), _) => 0
        // Those two below might be zero or something else than +- 1 as well
        case ((Some(_), Some(_)), None) => 0 //1
        case ((Some(_), None), Some(_)) => 0 //-1
        case ((Some(_), None), None) => 0
        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) if hasLexArgument(frameArg) => 10
        case _ => 0
      })
      //println(s"triple: $triple;; ${triple._1._2.map(x => hasLexArgument(x))} ${triple._2.map(x => hasLexArgument(x))} betterArg: $betterArg")
      i+=1
    }

    dprintln(s"SRVOTEDBG: ${frame.frameText} vs ${otherFrame.frameText} ---> score: $betterArg")

    betterArg -> debugValuesInfo
  }

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {

    def dprintln(s: String) = (); //if (math.abs(thr - 0.1)   < 0.000000001) println(s)

    val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

//    def filterFrames(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
//      val maybeFramesWithCounts = for (frame <- frames) yield {
//        val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame).map {
//          case Some((arg, walenty)) => Some(walenty)
//          case None => None
//        }
//        val matchingArgsFlatten = matchingArguments.flatten
//
//        /*println("\nxxxxx")
//        println(frame)
//        println(skladnicaArgumentsWithoutSie)
//        println(matchingArguments)
//        println("xxxx\n")*/
//
//        val frameHasLex = frame.arguments.exists(_.realizations.exists(t => t.contains("lex") || t.contains("comprep")))
//        val matchedHasLex = matchingArgsFlatten.exists(r => r.realizations.exists(t => t.contains("lex") || t.contains("comprep")))
//
//        // TODO: I could use filterer! For some reason there are frames with only lex?
//        if (frameHasLex && !matchedHasLex) {
//          None
//        } else {
//          Some((matchingArgsFlatten.size , frame , frameHasLex))
//        }
//      }
//      val framesWithCounts = maybeFramesWithCounts.flatten
//
//
//      val maxCount = if (framesWithCounts.nonEmpty) framesWithCounts.map(_._1).max else -1
////      val maxCount = if (framesWithCounts.nonEmpty) framesWithCounts.map(_._1).max else -1
//
//      val maxFrames = framesWithCounts.filter(f => f._1 == maxCount || f._3).map(_._2)
//
//      maxFrames
//    }


    def chooseFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]): (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
      val filFrames = frames //, skladnicaArgumentsWithoutSie)

      val framesWithMatchedArgs = for (frame <- filFrames) yield {
          val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        //val argumentsMatching = skladnicaArgumentsWithoutSie.zip(matchingArguments)

        val otherFormatArgumentsMatching = matchingArguments.map {
          case Some((arg, walentyArg)) => (Some(arg), Some(walentyArg))
          case None => (None, None)
        }

        (frame, otherFormatArgumentsMatching, matchingArguments)
      }

      val availableWalentyFrames = framesWithMatchedArgs.map(_._1)
      // nie potrzebne, ale zostawiam to na ewentualne uproszczenie pozniej...
      val swapperSeq: Seq[String => String] = Seq(identity[String] _)

      val results = swapperSeq.view.map { swapper =>
        var maxValue = 0
        var maxFrame: Option[WalentyFrame] = None
        var maxMatchingDebug = Seq[Option[WalentyArgument]]()
        var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None


        for ((frame, argumentsMatching, am) <- framesWithMatchedArgs) {
          var votes = 0
          val votesDebug = ArrayBuffer[Int]()
          val (voteScores, detailedDebug) = scoreFrame(words, frame, argumentsMatching, frame, argumentsMatching)
            votesDebug += voteScores

            votes = voteScores
          //}

          val l = List(predicateBase, aspect, frame.frameText, votes.toString)
          val dbgList = l ++ detailedDebug.flatMap { x => List(x._1, x._2, x._3, x._4.toString, x._5.toString, x._6, x._7) }
          // Random behaviour!
          //if (votes != 0)
          //  votes = ThreadLocalRandom.current().nextInt()

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), dbgList)

          if (votes > maxValue ) {
          //  if (votes > maxValue || filFrames.size == 1) {
            maxValue = votes
            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(am)
            events += event.copy(info = s"NEW_MAX_FRAME (votes: $votes)")
          } else if (votes == maxValue && maxValue > 0) {
            maxFrame = None
            maxMatchingDebug = Seq.empty
            maxPairs = None
            events += event.copy(info = s"NOT_MAX_FRAME_2 (votes: $votes)")
          } else {
            events += event.copy(info = s"NOT_MAX_FRAME (votes: $votes, max: $maxValue, [${votesDebug.mkString(",")}]})")
          }

        }


        val debugInfo = MaximalFrameChooserDebugInfo(events)
        maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size, maxPairs.get)) -> debugInfo
      }
      results.find(_._1.isDefined).getOrElse {
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}
