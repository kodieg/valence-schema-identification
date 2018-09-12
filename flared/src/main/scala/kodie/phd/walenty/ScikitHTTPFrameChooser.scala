package kodie.phd.walenty

import java.util.concurrent.ConcurrentHashMap

import experimental.{SchemaFeatureExtractor, ArgumentWithProb}
import kodie.phd.skladnica.types.Word

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.util.control.NonFatal

import scalaj.http._
object Addresses {
  val addresses = Seq.tabulate(21) { i =>
    s"http://localhost:${5100 + i}/prob"
  }
}

object Cache {
  import scala.collection.JavaConverters._
  val cache = new ConcurrentHashMap[(String, String, String, Int), Array[Option[Float]]]().asScala
}

class ScikitHTTPFrameChooser(walenty: NewWalentyParser, featureExtractor: SchemaFeatureExtractor, frameSwapperBuilder: FrameSwapperBuilder, addr: Seq[String] = Addresses.addresses, useCache: Boolean = false) {

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], useSwapper: Boolean, thr: Double = 0.1, ratio: Double = 5.0) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {
    val events = mutable.ArrayBuffer[MaximalFrameChooserDebugEvent]()

    def filterFrames(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
      val maybeFramesWithCounts = for (frame <- frames) yield {
        val matchingArguments = WalentyArgumentMatcher.matchingElements(words, skladnicaArgumentsWithoutSie, frame).map {
          case Some((arg, walenty)) => Some(walenty)
          case None => None
        }
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

    def calculateProbability(frames: Seq[WalentyFrame]) : Array[Option[Float]] = {
      // TODO: cache does not support frame swapper!
      val key = (predicateBase, aspect, words.map(_.orth).mkString, frames.size)
      if (useCache) {
        val cacheHit = Cache.cache.get(key)
        if (cacheHit.isDefined) {
          return cacheHit.get
        }
      }

      val reqBody = frames.map { frame =>
        val feats = featureExtractor.extract(Word(predicateBase, predicateBase, "TODO..."), words, skladnicaArguments, frame, -1)
        val headers = featureExtractor.headers(frame)

        s"${headers.replaceAll("\n", "")}\n${feats.replaceAll("\n", "")}\n"
      }.mkString

      def doRequest() = {
        val a = addr(ThreadLocalRandom.current().nextInt(addr.size))

        val resps = Http(a).postData(reqBody).timeout(connTimeoutMs = 100000, readTimeoutMs = 110000).header("content-type", "application/text").asString.body
        val z = resps.split('\n').map { resp =>
          val res = resp.trim()
          if (res == "NaN" || res.trim.isEmpty) None
          else Some(res.toFloat)
        }
        if (z.size != frames.size) {
          println("INVALID RESPONSE SIZE!!!!: " + reqBody)
          frames.map(_ => None.asInstanceOf[Option[Float]]).toArray
        } else {
          z
        }

      }

      if (frames.isEmpty) return Array.empty[Option[Float]]
      else {
        var i = 0

        while (i < 5) {
          try {
            val r =  doRequest()
            if (useCache) Cache.cache += (key -> r)
            return r
          } catch {
            case NonFatal(e) =>
              println(s"Could not get probabilities: $e")
              i += 1
          }
        }
        println(s"Getting probabilities failed!:q" +
          s"")
        return Array.empty[Option[Float]]
      }

    }

    def chooseFrameFrom(frames: Seq[WalentyFrame], skladnicaArgumentsWithoutSie: Seq[ArrayBuffer[ArgumentWithProb]]) = {
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
      // should I use frame swapper?!
      val swapperSeq: Seq[String => String] = if (useSwapper && availableWalentyFrames.nonEmpty) frameSwapperBuilder.buildSwapper(availableWalentyFrames, Some(predicateBase)) else Seq(identity[String] _)

      val results = swapperSeq.view.map { swapper =>
        var maxFrame: Option[WalentyFrame] = None
        var maxMatchingDebug = Seq[Option[WalentyArgument]]()
        var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None

        val probs = calculateProbability(framesWithMatchedArgs.map(_._1))

        val res = for (((frame, argumentsMatching, am), votesOpt) <- framesWithMatchedArgs.zip(probs)) yield {
          val votesDebug = ArrayBuffer[Int]()
          val votes = votesOpt.getOrElse(0.0f)  // TODO: try to use info that no model was available!

          val l = List(predicateBase, aspect, frame.frameText, votes.toString)

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), l)

          (frame, event, votes, argumentsMatching, am)
        }

        if (res.nonEmpty && probs.forall(_.isDefined)) {

          val sorted = res.sortBy(-_._3)
          val maxValue = sorted.head._3
          val frame = sorted.head._1
          val argumentsMatching = sorted.head._4
          val event = sorted.head._2

          val secondMax = sorted.lift(1).map(_._3).getOrElse(-1.0f)

          if (math.abs(maxValue - secondMax) > thr && (maxValue) > ratio && secondMax >= 0.0) {
            //if (math.abs(maxValue - secondMax) > 0.1) {

            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(sorted.head._5)

            events += event.copy(info = s"NEW_MAX_FRAME (votes: $maxValue)")
            //println(s"Chosen max frame by scikit with prob: $maxValue (second: $secondMax) (params: $thr, $ratio)")
          }

          sorted.foreach { case (frame, event, votes, argumentsMatching, _) =>
            if (Some(frame) != maxFrame) {
              events += event.copy(info = s"NOT_MAX_FRAME (votes: $votes, max: $maxValue)})")
            }
          }
        }

        val debugInfo = MaximalFrameChooserDebugInfo(events)
        maxFrame.map(frame => MaximalFrameChooserResult(frame, Seq.empty, frames.size, maxPairs.get)) -> debugInfo
      }
      results.find(_._1.isDefined).getOrElse {
        if (useSwapper) println(s"FRAMESWAPPER ALL ${results.size} failed!")
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}


