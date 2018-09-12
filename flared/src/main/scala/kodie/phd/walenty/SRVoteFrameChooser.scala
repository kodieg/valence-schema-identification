package kodie.phd.walenty

import experimental.{ArgumentWithProb, Argument}
import kodie.phd.skladnica.types.Word
import phd.sr.counts.BaseCounts
import phd.sr.data.WordsIndex
import phd.sr.scorer.pd.{PseudoDisambugiationInstance, ScoreBasedVerbClassifier, VerbPseudoDisambiguationInstance}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created by kodie on 9/8/15.
 */
class SRVoteFrameChooser(index: WordsIndex, walenty: NewWalentyParser, classifiers: Seq[ScoreBasedVerbClassifier], frameSwapperBuilder: FrameSwapperBuilder, baseCounts: BaseCounts) extends SRBaseChooser with Serializable {

  def scoresDifferenceIsRelevant(thr: Double)(score1: Double, score2: Double): Boolean = {
    // Let's start from simplicistic function :)
    // I think of doing abs(score1 - score2) / max(0.001, min(score1, score2)) > X (e.g. 0.1)
    // This below gave a lot of improvement to precision! (minor loss to recall)
    //println(s"$thr $score1 $score2"
    // )
    // Kluczowe porównywanie tylko scorów nie zerowych! inaczej preferuje
    Math.abs(score1 - score2) > thr// && score1 > 0.0 && score2 > 0.0
    //  Math.abs(score1 - score2) / (Math.max(Math.min(score1, score2), 0.001)) > 0.1
  }

  def normalize(vote: Int) = {
    // signum -- another option is to use: vote / classifiers.size
    if (vote > 0) 1
    else if (vote < 0) -1
    else 0
  }

  def frameIsBetherThanOther(words: Seq[Word], frame: WalentyFrame, matchedArgs: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])], otherFrame: WalentyFrame, otherMatchedArgs: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])], frameSwapper: String => String, thr: Double) = {
    val size = matchedArgs.size
    val p1 = index.getIndexOrElse(frameSwapper(frame.frameText), -1)
    val p2 = index.getIndexOrElse(frameSwapper(otherFrame.frameText), -1)
    val instanceTemplate = VerbPseudoDisambiguationInstance(-1, index.getIndexOrElse(frameSwapper(frame.frameText), -1), index.getIndexOrElse(frameSwapper(otherFrame.frameText), -1), 0, 0, -1, 0)

    val allArgs = matchedArgs.zip(otherMatchedArgs.map(_._2))
    var debugValuesInfo: List[(String, String, String, String, String, String, String)] = Nil

    def hasLexArgument(arg: WalentyArgument) = arg.realizations.exists(_.startsWith("lex")) || arg.realizations.exists(_.startsWith("comprep")) || arg.realizations.exists(_.startsWith("fixed"))

    def dprintln(s: String) = (); //if (math.abs(thr - 0.1)   < 0.000000001) println(s)

    var betterArg = 0.0
    var i = 0
    var hasSRAnyChoice = false
    for (triple <- allArgs) {

      //println(triple)
      betterArg += (triple match {
        case ((None, _), _) => 0
        // Those two below might be zero or something else than +- 1 as well
        case ((Some(_), Some(_)), None) => 0.5 //1 ???
        case ((Some(_), None), Some(_)) => -0.5 //-1 ??? todo: force any choice by sr?
        case ((Some(_), None), None) => 0
        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) if hasLexArgument(frameArg) && !hasLexArgument(otherFrameArg) =>
          val headBase = words(argument.semanticHead.getOrElse(i)).base
          debugValuesInfo = (argument.argumentType, frameArg.text, headBase, "1", "10", "", "") :: debugValuesInfo
          10
        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) if !hasLexArgument(frameArg) && hasLexArgument(otherFrameArg) => -10

        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) => //if argument.argumentType.startsWith("prep") =>
          val headBase = words(argument.semanticHead.getOrElse(i)).base
          val headBaseIndex = index.getIndexOrElse(headBase, -1)
          val groupType = index.getIndexOrElse(frameArg.text, -1)
          val otherGroupType = index.getIndexOrElse(otherFrameArg.text, -1)

          val instance = instanceTemplate.copy(
            semanticHeadLemma = headBaseIndex,
            semanticHeadOrth = headBaseIndex,
            groupType = groupType,
            _gt2 = otherGroupType
          )

          var argVote = 0
          for (classifier <- classifiers) {
            val classifierVote =  {
              val (result, (one, two)) = classifier.synchronized { classifier.pickWithDebug(instance) }

              //println(classifier.getClass.getSimpleName, instance, result, one, two)
              //if (one._2 > 0.0 && two._2 > 0.0 && thr < 0.5) println(s"SRVOTEDBG: $argument -- $headBase -- ${frame.frameText} -- ${frameArg.text} --> $one ::: ${otherFrame.frameText} -- ${otherFrameArg.text} -> $two")

              val g1Cnt = baseCounts.predicateGroupCounts.getOrElse((p1, groupType), 0.0f)
              val g2Cnt = baseCounts.predicateGroupCounts.getOrElse((p2, otherGroupType), 0.0f)

              // HERE: testing min group cnt requirements  istead of requiring score > 0 for each item
              val v = if (p1 != -1 && p2 != -1 && g1Cnt > 20 && g2Cnt > 20 ) {
                (scoresDifferenceIsRelevant(thr)(one._2, two._2) -> result) match {
                  case (true, PseudoDisambugiationInstance.CORRECT_IS_ONE) => 1
                  case (true, PseudoDisambugiationInstance.CORRECT_IS_TWO) => -1
                  case _ => 0
                }
              } else {
                0
              }
              debugValuesInfo = (argument.argumentType, frameArg.text, headBase, v.toString, one._2.toString, "", "") :: debugValuesInfo
              if (v != 0) {
                hasSRAnyChoice = true
              }
              v
              // Random behaviour for sanity test


/*
              val (one1, two1) = (Random.nextDouble(), Random.nextDouble())

              if (one1 > two1) 1
              else -1
*/
            }


            argVote += classifierVote
            //println("argVote: " + argVote)
          }
          val n = normalize(argVote)
          //println(s"normalized: $n (argVote: $argVote)")
          n
        case _ => 0
      })
      dprintln(s"SRVOTEDBG: ${triple._1._1} --> current score: $betterArg")
      //println(s"triple: $triple;; ${triple._1._2.map(x => hasLexArgument(x))} ${triple._2.map(x => hasLexArgument(x))} betterArg: $betterArg")
      i+=1
    }

    dprintln(s"SRVOTEDBG: ${frame.frameText} vs ${otherFrame.frameText} ---> score: $betterArg")

    //(if (hasSRAnyChoice) betterArg else 0 ) -> debugValuesInfo //???? to daje gorsze wyniki niz random :(
    betterArg -> debugValuesInfo
  }

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]]) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    chooseFrame(predicateBase, aspect, words, skladnicaArguments, useSwapper = false)
  }

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], useSwapper: Boolean, thr: Double = 0.1) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {

    def dprintln(s: String) = (); //if (math.abs(thr - 0.1)   < 0.000000001) println(s)

    val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

    def filterFrames(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
      val maybeFramesWithCounts = for (frame <- frames) yield {
        val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame).map {
          case Some((arg, walenty)) => Some(walenty)
          case None => None
        }
        val matchingArgsFlatten = matchingArguments.flatten

        /*println("\nxxxxx")
        println(frame)
        println(skladnicaArgumentsWithoutSie)
        println(matchingArguments)
        println("xxxx\n")
        */

        val frameHasLex = frame.arguments.exists(_.realizations.exists(t => t.contains("lex") || t.contains("comprep")))
        val matchedHasLex = matchingArgsFlatten.exists(r => r.realizations.exists(t => t.contains("lex") || t.contains("comprep")))

        // TODO: I could use filterer! For some reason there are frames with only lex?
        if (frameHasLex && !matchedHasLex) {
          None
        } else {
          Some((matchingArgsFlatten.size , frame , frameHasLex))
        }
      }
      val framesWithCounts = maybeFramesWithCounts.flatten


      val maxCount = if (framesWithCounts.nonEmpty) framesWithCounts.map(_._1).max else -1

      val maxFrames = framesWithCounts.filter(f => f._1 == maxCount || f._3).map(_._2)

      maxFrames
    }


    def chooseFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]): (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
      val filFrames = filterFrames(frames, skladnicaArgumentsWithoutSie)

      val framesWithMatchedArgs = for (frame <- filFrames) yield {
          val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame)
        //val argumentsMatching = skladnicaArgumentsWithoutSie.zip(matchingArguments)

        val otherFormatArgumentsMatching = matchingArguments.map {
          case Some((arg, walentyArg)) => (Some(arg), Some(walentyArg))
          case None => (None, None)
        }

        (frame, otherFormatArgumentsMatching, matchingArguments)
      }

      // TODO: support swapper?!
      val filFramesTrainingCounts = filFrames.map { wf =>
        baseCounts.predicateCounts.getOrElse(index.getIndexOrElse(wf.frameText, -1), 0.0f)
      }

//      val skipDueToCounts = /*filFrames.size > 1 &&*/ filFramesTrainingCounts.exists(_ < 100.0f)

      //println(filFrames.toList)
      // TODO: below thing is not good for final eval!  no bo moze byc tak ze jest lex() dopasowany!
      /*if (filFrames.size > 2) {
        return (None.asInstanceOf[Option[MaximalFrameChooserResult]], MaximalFrameChooserDebugInfo(Seq.empty))
      }*/

      dprintln(s"SRVOTEDBG: $predicateBase -- ${words.map(_.orth).mkString(" ")} -- $skladnicaArguments")

      val availableWalentyFrames = framesWithMatchedArgs.map(_._1)
      val swapperSeq: Seq[String => String] = if (useSwapper && availableWalentyFrames.nonEmpty) frameSwapperBuilder.buildSwapper(availableWalentyFrames, Some(predicateBase)) else Seq(identity[String] _)

      val results = swapperSeq.view.map { swapper =>
        var maxValue = 0.0
        var secondMax = 0.0
        var maxFrame: Option[WalentyFrame] = None
        var maxMatchingDebug = Seq[Option[WalentyArgument]]()
        var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None

        var detailedDebug: List[(String, String, String, String, String, String, String)] = Nil


        for ((frame, argumentsMatching, am) <- framesWithMatchedArgs) {
          var votes = 0
          val votesDebug = ArrayBuffer[Double]()
          for ((otherFrame, otherArgsMatching, _) <- framesWithMatchedArgs if frame != otherFrame) {
            val (voteScores, dd) = frameIsBetherThanOther(words, frame, argumentsMatching, otherFrame, otherArgsMatching, swapper, thr)
            votesDebug += voteScores
            if (voteScores > 0) {
              votes += 1
            }
            detailedDebug = dd
          }

          val l = List(predicateBase, aspect, frame.frameText, votes.toString)
          val dbgList = l ++ detailedDebug.flatMap { x => List(x._1, x._2, x._3, x._4.toString, x._5.toString, x._6, x._7) }

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), dbgList)

          if (votes > maxValue || filFrames.size == 1) {
            secondMax = maxValue
            maxValue = votes
            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(am)
            dprintln(s"SRVOTEDBG: ${frame.frameText} -- new max -- $votes")
            events += event.copy(info = s"NEW_MAX_FRAME (votes: $votes)")
          } else if (votes == maxValue) {
            maxFrame = None
            maxMatchingDebug = Seq.empty
            maxPairs = None
            events += event.copy(info = s"NOT_MAX_FRAME 2 (votes: $votes, max: $maxValue, [${votesDebug.mkString(",")}]})")
          }else {
            events += event.copy(info = s"NOT_MAX_FRAME (votes: $votes, max: $maxValue, [${votesDebug.mkString(",")}]})")
            dprintln(s"SRVOTEDBG: ${frame.frameText} -- not max -- $votes")
          }

        }

        dprintln(s"SRVOTEDBG:\n")

        val debugInfo = MaximalFrameChooserDebugInfo(events)

        //if (maxValue - secondMax > 1) {

          maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size, maxPairs.get)) -> debugInfo
        //} else {
        //  None -> debugInfo
        //}

      }
      results.find(_._1.isDefined).getOrElse {
        //if (useSwapper) println(s"SRVOTE FRAMESWAPPER ALL ${results.size} failed!")
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}
