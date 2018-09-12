package kodie.phd.walenty

import experimental.ArgumentWithProb
import kodie.phd.skladnica.types.Word
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import phd.sr.data.{PredicateGroupInstance, PredicateGroupDataset}
import phd.sr.data.PredicateGroupDataset.PredicateOrth

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Created by kodie on 2/4/16.
 */
class NaiveBayesArgumentsClassifier(raw: PredicateGroupDataset,index: phd.sr.data.WordsIndex, data: phd.sr.data.PredicateGroupCounter[Float]) {

  val frameCounts = data.flatMap { case (key, count) => Try {
    val frame = index.getWordOrElse(key.predicate, "xxx")
    val groupType = index.getWordOrElse(key.groupType, "xxx")
    val realPredicate = frame.substring(0, frame.indexOf(' '))
    // synset!

    (realPredicate, frame, count)
  }.toOption}.toSeq.groupBy(_._1).map { case (pred, seq) =>
    pred -> seq.groupBy(_._2).mapValues(_.map(_._3).sum).map(identity _)
  }

  val argCounts = data.flatMap { case (key, count) => Try {
    val frame = index.getWordOrElse(key.predicate, "xxx")
    val groupType = index.getWordOrElse(key.groupType, "xxx")
    val realPredicate = frame.substring(0, frame.indexOf(' '))
    // synset!

    (frame, groupType, count)
  }.toOption}.toSeq.groupBy(_._1).map { case (pred, seq) =>
    pred -> seq.groupBy(_._2).mapValues(_.map(_._3).sum).map(identity _)
  }


  // TODO: I cannot calculate this from this data?
  val globalArgs = mutable.Map[String, Float]().withDefaultValue(0.0f)

  val frameWithArg = mutable.Map[String, Int]().withDefaultValue(0)
  /**/

  {
    def handle(cur:  mutable.ArrayBuffer[PredicateGroupInstance], relargs: mutable.Set[String]): Unit = {
      val predicateOpt = index.getWord(cur.head.predicateLemma)

      predicateOpt.foreach { predicate =>
        val frPart = predicate.split(":", 6)(5)
        val args = BracketAwareSplitter('+', frPart).map(_.trim())
        for (a <- args)
          frameWithArg(a) += 1

        //frameWithArg(predicate) += 1
        args.toSet.intersect(relargs).foreach { arg =>
          globalArgs(arg) += 1.0f
        }
      }
    }




    var curList = mutable.ArrayBuffer[PredicateGroupInstance]()
    var prev: Option[PredicateOrth] = None
    var realizedArgs: mutable.Set[String] = mutable.Set()
    for (data <- raw) {
      index.getWord(data.groupType).foreach { gt =>
        //globalArgs(gt) += 1.0f
        realizedArgs += gt
      }
      //println(s"XXXXXX $prev $current $curList")
      val current = Option(data.predicateOrth)
      if (current == prev) {
        curList += data
      } else if (curList.nonEmpty) {
        handle(curList , realizedArgs)
        curList.clear()
        realizedArgs.clear()
        prev = current
      } else {
        prev = current
      }
    }
    if (curList.nonEmpty) handle(curList, realizedArgs)
  }

  //println("FRAMEWITHARGS", frameWithArg.toStream.take(10).toList)

  val globalArgsSize = globalArgs.values.size

  val totalFrames = frameCounts.mapValues(_.values.sum).map(identity _)
  val lambda = 0.1
  val totalWalentyFrames = 30000 // FIXME: real value

  //Backoff should be used after this... or when prob is missing?
  def backoffProb(arg: WalentyArgument) = {
    val txt = arg.text

    //println(s"BACKOFF: $txt ${(globalArgs.get(txt))} / ${frameWithArg.get(txt)}")
    (globalArgs.getOrElse(txt, 0.0f) + lambda) / (frameWithArg.getOrElse(txt, 0) + lambda * globalArgsSize)
  }

  // Calculates P(C)*product(P(x|C))
  def probNominator(frame: WalentyFrame, args: Set[WalentyArgument]) = {
    val realPredicate = frame.frameText.substring(0, frame.frameText.indexOf(' '))
    val frameProb = math.log(frameCounts.getOrElse(realPredicate, Map()).getOrElse(frame.frameText, 0.0f) + lambda) / (totalFrames.getOrElse(realPredicate, 0.0f) + lambda * totalWalentyFrames) // TODO: add one requires fixes in normalization!
    val allArgs = argCounts.getOrElse(frame.frameText, Map()).map(_._2).sum + lambda * frame.arguments.size
    val argsProb = args.map { argument =>
      val cnt = argCounts.getOrElse(frame.frameText, Map()).getOrElse(argument.text, 0.0f)
      //println(s"XXXXX ${ argCounts.getOrElse(frame.frameText, Map())}... ${argCounts.keys.take(10).toList}  ")
      if (true /* && cnt != 0*/) {
        //println(s"HERE! $cnt")
        val argCnt = cnt + lambda // TODO: I could use some average/global counts as backoff
        val argProb = argCnt / allArgs
        math.log(argProb)
      } else {
        // backoff w zasadzie daje taki model, że pogarsza :/
        // dlatego jest wyłączony!
        val b = math.log(backoffProb(argument))
        //println(s"BACKOFF FOR $argument == $b")
        b
      }
    }.sum

    val totalProbA = frameProb + argsProb
    totalProbA -> (frameProb, argsProb)
  }

  def prob(frame: WalentyFrame, allPossible: Seq[WalentyFrame], args: Set[WalentyArgument]) = {
    require(allPossible.contains(frame))

    //println("framecounts", frameCounts)
    //println("argcounts", argCounts)

    val (totalProbA, (frameProb, argsProb)) = probNominator(frame, args)
    val otherProbs = allPossible.map(probNominator(_, args)._1)//.sum
    /*
    // To chyba nie jest poprawnie polczona normalizacja (chyba powinna byc jakas \sum...)
    /val z = args.map { argument =>
      math.log(globalArgs.getOrElse(argument.text, 0) + lambda) / (globalArgsSum + lambda * frame.arguments.size) // I'm not sure wether this normalization is correct
    }.sum
    */

    val z = /*totalProbA +*/ math.log(otherProbs.map(math.exp _).sum)

    val probVal = math.exp(totalProbA - z)
    //println(s"PROBDEBUG P(${frame.frameText}|(${args.size})${args.map(_.text).mkString(",")} = ${math.exp(frameProb)}*${math.exp(argsProb)}/${math.exp(z)} = $probVal")

    probVal
  }
}


class NaiveBayesFrameChooser(walenty: NewWalentyParser, classifier: NaiveBayesArgumentsClassifier, frameSwapperBuilder: FrameSwapperBuilder) {

  def chooseFrame(predicateBase: String, aspect: String, words: Seq[Word], skladnicaArguments: Seq[ArrayBuffer[ArgumentWithProb]], useSwapper: Boolean, thr: Double = 0.1, ratio: Double = 5.0, onlyMaxFrames: Boolean) : (Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo) = {


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

      val maxFrames = if (onlyMaxFrames) framesWithCounts.filter(_._1 == maxCount).map(_._2) else framesWithCounts.filter(_._1 > 0).map(_._2)

      maxFrames
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


        val res = for ((frame, argumentsMatching, am) <- framesWithMatchedArgs) yield {
          val votesDebug = ArrayBuffer[Int]()
          val votes = classifier.prob(frame, frames, argumentsMatching.flatMap(_._2).toSet)
          val l = List(predicateBase, aspect, frame.frameText, votes.toString)

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), l)

          (frame, event, votes, argumentsMatching, am)
        }

        if (res.nonEmpty) {

          val sorted = res.sortBy(-_._3)
          val maxValue = sorted.head._3
          val frame = sorted.head._1
          val argumentsMatching = sorted.head._4
          val event = sorted.head._2

          val secondMax = sorted.lift(1).map(_._3).getOrElse(-1.0)

          // TOO: maybe &&? maybe maxValue > XX not ratio!
          if (math.abs(maxValue - secondMax) > thr || (maxValue / secondMax) > ratio) {
            //if (math.abs(maxValue - secondMax) > 0.1) {

            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(sorted.head._5)

            events += event.copy(info = s"NEW_MAX_FRAME (votes: $maxValue)")
          }

          sorted.foreach { case (frame, event, votes, argumentsMatching, _) =>
            if (Some(frame) != maxFrame) {

              events += event.copy(info = s"NOT_MAX_FRAME (votes: $votes, max: $maxValue)})")
            }
          }
        }


        val debugInfo = MaximalFrameChooserDebugInfo(events)
        maxFrame.map(frame => MaximalFrameChooserResult(frame, Seq.empty, frames.size, maxPairs.get )) -> debugInfo
      }
      results.find(_._1.isDefined).getOrElse {
        //if (useSwapper) println(s"FRAMESWAPPER ALL ${results.size} failed!")
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}


class NegativeNaiveBayesFrameChooser(walenty: NewWalentyParser, classifier: NaiveBayesArgumentsClassifier) {

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
      val swapperSeq: Seq[String => String] = /*if (useSwapper && availableWalentyFrames.nonEmpty) frameSwapperBuilder.buildSwapper(availableWalentyFrames, Some(predicateBase)) else */Seq(identity[String] _)

      val results = swapperSeq.view.map { swapper =>
        var maxFrame: Option[WalentyFrame] = None
        var maxMatchingDebug = Seq[Option[WalentyArgument]]()
        var maxPairs: Option[Seq[Option[(ArgumentWithProb, WalentyArgument)]]] = None


        val res = for ((frame, argumentsMatching, am) <- framesWithMatchedArgs) yield {
          val votesDebug = ArrayBuffer[Int]()
          val matchedArgs = argumentsMatching.flatMap(_._2).toSet
          val unmatched = frame.arguments.toSet diff matchedArgs
          // TODO does this formulation (1-p(unmatched))*p(matched)? makes sense?
          // maybe some other formulation would make more sense?(e.g. somehow joing single unmatched p(arg))
          val unmpr = (1 - classifier.prob(frame, frames, unmatched))
          val votes = unmpr * classifier.prob(frame, frames, matchedArgs)

          val l = List(predicateBase, aspect, frame.frameText, votes.toString)

          val event = MaximalFrameChooserDebugEvent(frame, "", argumentsMatching, swapper(frame.frameText), l)

          (frame, event, votes, argumentsMatching, unmpr, am)
        }

        if (res.nonEmpty) {

          val sorted = res.sortBy(-_._3)
          val maxValue = sorted.head._3
          val frame = sorted.head._1
          val argumentsMatching = sorted.head._4
          val event = sorted.head._2

          val secondMax = sorted.lift(1).map(_._3).getOrElse(-1.0)

          if (math.abs(maxValue - secondMax) > thr || (maxValue / secondMax) > ratio) {
            //if (math.abs(maxValue - secondMax) > 0.1) {

            maxFrame = Some(frame)
            maxMatchingDebug = argumentsMatching.map(_._2)
            maxPairs = Some(sorted.head._6)
            events += event.copy(info = s"NEW_MAX_FRAME (votes: $maxValue)")
          }

          sorted.foreach { case (frame, event, votes, argumentsMatching, unmr, _) =>
            if (Some(frame) != maxFrame) {

              events += event.copy(info = s"NOT_MAX_FRAME (votes: $votes, unmatched: $unmr max: $maxValue)})")
            }
          }
        }

        val debugInfo = MaximalFrameChooserDebugInfo(events)
        maxFrame.map(frame => MaximalFrameChooserResult(frame, Seq.empty, frames.size,maxPairs.get)) -> debugInfo
      }
      results.find(_._1.isDefined).getOrElse {
        //if (useSwapper) println(s"FRAMESWAPPER ALL ${results.size} failed!")
        results.head
      }
    }


    val hasSieArgument = skladnicaArguments.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
    val possibleFrames = walenty.framesFor(predicateBase, aspect, hasSieArgument)

    chooseFrameFrom(possibleFrames, skladnicaArguments.map(_.filter(_.argumentType != "sie")))
  }
}