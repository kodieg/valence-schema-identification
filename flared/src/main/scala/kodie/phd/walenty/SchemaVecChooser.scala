package kodie.phd.walenty

import breeze.linalg.DenseVector
import experimental.ArgumentWithProb
import kodie.phd.skladnica.types.Word
import phd.sr.counts.BaseCounts
import phd.sr.data.WordsIndex
import phd.sr.scorer.pd.{PseudoDisambugiationInstance, VerbPseudoDisambiguationInstance}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.ThreadLocalRandom

/**
 * Created by kodie on 9/20/16.
 */
class SchemaVecChooser(walenty: NewWalentyParser, baseCounts: BaseCounts, index: WordsIndex, similarity: VectorBasedSR, word2vec: Map[String, DenseVector[Double]], frameSwapperBuilder: FrameSwapperBuilder) extends Serializable {
  val ZERO = DenseVector.zeros[Double](300)
  def scoreFrame(words: Seq[Word], frame: WalentyFrame, matchedArgs: Seq[(Option[ArgumentWithProb], Option[WalentyArgument])], frameSwapper: String => String): (Double, List[(String, String, String, String, String, String, String)]) = {
    val size = matchedArgs.size
    val p1 = index.getIndexOrElse(frameSwapper(frame.frameText), -1).toLong

    var debugValuesInfo: List[(String, String, String, String, String, String, String)] = Nil

    var i = 0
    var argsCnt = 0.0
    var totalScore = 0.0
    for (arg <- matchedArgs) {
      val argScore : Double = arg match {
        case (Some(a), Some(arg)) =>
          val groupType = index.getIndexOrElse(arg.text, -1)
          val wordO = a.semanticHead
          argsCnt += 1
          if (groupType != -1) {
            val word = words(wordO.getOrElse(i)).orth // taking orth may actually make sense here...
            val wordVec = word2vec.getOrElse(word, ZERO)
            /*val frameVec = frameVecs.getOrElse(p1 * SchemaVectorsProviders.FACTOR + groupType, ZERO)

            val d = frameVec dot wordVec
            val n = math.sqrt((frameVec dot frameVec) * (wordVec dot wordVec))
*/
            val ss = similarity.computeSimilarity(p1, groupType, wordVec)
            //println(s"${frame.frameText} -- Argument ${a.argumentType} -->   $word --> $ss")
            debugValuesInfo = (a.argumentType, arg.text, word, ss.toString, "", "", "") :: debugValuesInfo

            ss
          } else {
            0.0
          }

        case _ => 0.0
      }
      totalScore += argScore
      i += 1
    }
    return (totalScore / argsCnt) -> debugValuesInfo
    /*def hasLexArgument(arg: WalentyArgument) = arg.realizations.exists(_.startsWith("lex")) || arg.realizations.exists(_.startsWith("comprep")) || arg.realizations.exists(_.startsWith("fixed"))

    def dprintln(s: String) = (); //if (math.abs(thr - 0.1)   < 0.000000001) println(s)

    var betterArg = 0
    var i = 0
    for (triple <- allArgs) {

      //println(triple)
      betterArg += (triple match {
        case ((None, _), _) => 0
        // Those two below might be zero or something else than +- 1 as well
        case ((Some(_), Some(_)), None) => 0 //1
        case ((Some(_), None), Some(_)) => 0 //-1
        case ((Some(_), None), None) => 0
        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) if hasLexArgument(frameArg) && !hasLexArgument(otherFrameArg) => 10
        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) if !hasLexArgument(frameArg) && hasLexArgument(otherFrameArg) => -10

        case ((Some(argument), Some(frameArg)), Some(otherFrameArg)) /*if argument.argumentType.startsWith("prep")*/ =>
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
            val classifierVote = classifier.synchronized {
              val (result, (one, two)) = classifier.pickWithDebug(instance)

              //println(classifier.getClass.getSimpleName, instance, result, one, two)
              //if (one._2 > 0.0 && two._2 > 0.0 && thr < 0.5) println(s"SRVOTEDBG: $argument -- $headBase -- ${frame.frameText} -- ${frameArg.text} --> $one ::: ${otherFrame.frameText} -- ${otherFrameArg.text} -> $two")

              val g1Cnt = baseCounts.predicateGroupCounts.getOrElse((p1, groupType), 0.0f)
              val g2Cnt = baseCounts.predicateGroupCounts.getOrElse((p2, otherGroupType), 0.0f)

              // HERE: testing min group cnt requirements  istead of requiring score > 0 for each item
              if (p1 != -1 && p2 != -1 && g1Cnt > 10 && g2Cnt > 10 ) {
                (scoresDifferenceIsRelevant(thr)(one._2, two._2) -> result) match {
                  case (true, PseudoDisambugiationInstance.CORRECT_IS_ONE) => 1
                  case (true, PseudoDisambugiationInstance.CORRECT_IS_TWO) => -1
                  case _ => 0
                }
              } else {
                0
              }

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

    betterArg*/
  }

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], chooseRandomFrame: Boolean, thr: Double, minFrameCount: Double, useSwapper: Boolean) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
  /*  chooseFrame(predicateBase, aspect, words, skladnicaArguments,  chooseRandomFrame, useSwapper = useSwapper, thr=thr,  minFrameCount = minFrameCount)
  }

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], chooseRandomFrame: Boolean, useSwapper: Boolean, thr: Double = 0.1, minFrameCount: Double = 0.0) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
*/
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

      val availableWalentyFrames = framesWithMatchedArgs.map(_._1)
      val swapperSeq: Seq[String => String] = if (useSwapper && availableWalentyFrames.nonEmpty) frameSwapperBuilder.buildSwapper(availableWalentyFrames, Some(predicateBase)) else Seq(identity[String] _)

      val results = swapperSeq.view.map { swapper =>
        // IDEAS::: max value = -10000 ---> this would allow to do similar thing as one vote!
        //      ::: minimal difference threshold (i.e best score - second score > thr
        //      ::: minimal avg(argument score) (i.e. maxScore / numArgs > thr2)
        //      ::: limit to 2-3 max schemas (return None if filFrames.size > 2/3)...
        //      ::: introduce some limitations using base counts (e.g. min verb count, or min frame/frames counts)
        var maxValue = 0.0
        var secondValue = maxValue
        var maxFrame: Option[WalentyFrame] = None
        var maxMatchingDebug = Seq[Option[WalentyArgument]]()
        var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None

        val forAllHave = filFrames.forall { frame =>
          val frameCount = baseCounts.predicateCounts.getOrElse(index.getIndexOrElse(frame.frameText, -1000), 0.0f)
          frameCount >= minFrameCount
        }

        val framesWithMatchedArgsX = if (forAllHave) framesWithMatchedArgs else Seq.empty

        for ((frame, argumentsMatching, am) <- framesWithMatchedArgsX) {
          val frameCount = baseCounts.predicateCounts.getOrElse(index.getIndexOrElse(frame.frameText, -1000), 0.0f)

          val votesDebug = ArrayBuffer[Int]()
          val (votes, argDebug) = if (!chooseRandomFrame) {
            if (frameCount >= minFrameCount) {
              scoreFrame(words, frame, argumentsMatching, swapper)
            } else {
              (0.0, List.empty)
            }
          } else {
            if (scoreFrame(words, frame, argumentsMatching, swapper) != 0.0)
              (ThreadLocalRandom.current().nextDouble(), List.empty)
            else (0.0, List.empty)
          }

          val l = List(predicateBase, aspect, frame.frameText, votes.toString)
          val dbgList = l ++ argDebug.flatMap { x => List(x._1, x._2, x._3, x._4.toString, x._5.toString, x._6, x._7) }

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), dbgList)

          if (votes > maxValue || filFrames.size == 1) {
            secondValue = maxValue
            maxValue = votes
            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(am)
            dprintln(s"VEC: ${frame.frameText} -- new max -- $votes")
            events += event.copy(info = s"NEW_MAX_FRAME (vec: $votes)")
          } else if (votes == maxValue) {
            events += event.copy(info = s"NOT_MAX_FRAME BUT EQUAL (vec: $votes, max: $maxValue, [${votesDebug.mkString(",")}]})")
            secondValue = votes
          } else {
            events += event.copy(info = s"NOT_MAX_FRAME (vec: $votes, max: $maxValue, [${votesDebug.mkString(",")}]})")
            dprintln(s"VEC: ${frame.frameText} -- not max -- $votes")
          }

        }

        dprintln(s"VEC:\n")
        if (maxValue - secondValue < thr) {
          maxFrame = None
        }

        val debugInfo = MaximalFrameChooserDebugInfo(events)
        maxFrame.map(frame => MaximalFrameChooserResult(frame, maxMatchingDebug, frames.size, maxPairs.get)) -> debugInfo
      }
      results.find(_._1.isDefined).getOrElse {
        //if (useSwapper) println(s"VEC FRAMESWAPPER ALL ${results.size} failed!")
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}
