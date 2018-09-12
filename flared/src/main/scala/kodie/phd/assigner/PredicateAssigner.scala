package kodie.phd.assigner

import kodie.phd.skladnica.types._
import kodie.phd.walenty.flat.FlatWalentyInstnce
import scala.collection.mutable
import scala.util.Try
import kodie.phd.skladnica.phrases.CONFIG
import scala.io.{Codec, Source}

/**
 * Created by kodie on 9/24/14.
 */
trait PredicateAssigner extends PredAssignerHelper with Serializable {
  def assign(words: Seq[Word], arguments: Seq[String]) : Seq[Option[(Span, String)]]

  def findPredicate(words: Seq[Word], arguments: Seq[String], extractInfP: Boolean = true): Seq[Span] =
    PredicateAssigner.findPredicates(words, arguments, extractInfP)


}

object PredicateAssigner extends PredAssignerHelper {}


trait PredAssignerHelper  {
  def findPredicates(words: Seq[Word], arguments: Seq[String], extractInfP: Boolean = true): Seq[Span] = {
    words.zipWithIndex.flatMap {
      case (word, index) =>
        // exclude infinitive verbs if they were marked as infp argument (correct?)
        val infpFilter = (CONFIG.EXTRACT_ARGUMENTS_OF_INFP && extractInfP) || !(arguments(index).startsWith("infp")) //|| (index > 1 && (words(index-1).ctag.startsWith("inf"))))
        if (isPredicate(word) && infpFilter //&& !(arguments(index).startsWith("infp") || (index > 1 && (words(index-1).ctag.startsWith("inf"))))
          && auxFilter(words, arguments, index) && infFilter(words, arguments, index))
          Some(Span(index, index + 1))
        else
          None
    }
  }

  val VERB_POS = Set("inf", "praet", "fin",  "impt", "imps", "bedzie", "pred", "winien")
  val AUX = Set("będzie", "było", "będą", "były", "byli", "był", "była", "będziemy")

  def auxFilter(words: Seq[Word], args: Seq[String], index: Int) : Boolean = {
    val predicateNearby = (index > 1 && isPredicateNotInf(words(index-1))) || (index > 2 && isPredicateNotInf(words(index-2)) && !Seq("i", "oraz", "bądź", "lub", ",", ";").contains(words(index-1).base) ) || (index + 1 < words.size && isPredicateNotInf(words(index+1))) || (index + 2 < words.size && isPredicateNotInf(words(index+2)) && !Seq("i", "oraz", "bądź", "lub", ",", ";").contains(words(index+1).base) )
    return !(AUX.contains(words(index).orth) && predicateNearby)
  }

  def infFilter(words: Seq[Word], args: Seq[String], index: Int) : Boolean = {
    return true
  }

  def isPredicateNotInf(word: Word) = {
    isPredicate(word) //&& !word.ctag.startsWith("inf")
  }

  def isPredicate(word: Word) = {
  val pos = word.ctag.split(":",2)(0)
    VERB_POS.contains(pos)
  }

  def evaluate(assigner: PredicateAssigner, sentenceArgsPairs: Traversable[(Sentence, Array[String])]) = {
    var totalForRecall = 0
    var totalForPrecision = 0
    var correct = 0

    var sentCorrect = 0
    var sentTotal = 0

    for ((sentence, args) <- sentenceArgsPairs)  {
      val assignment = assigner.assign(sentence.words, args)
      import kodie.phd.skladnica.features.extractBothArgumentWithPredicate
      val gold = extractBothArgumentWithPredicate(sentence)

      var allCorrect = true
      sentTotal += 1

      for (i <- 0 until sentence.words.size) {

        val goldArgOpt = gold(i)
        val assignmentArgOpt = assignment(i)

        (goldArgOpt, assignmentArgOpt) match {
          case (None, None) => ;
          case ((Some(_)), None) => allCorrect = false; totalForRecall += 1
          case (None, Some(_)) => allCorrect = false; totalForPrecision += 1
          case (Some(goldArg), Some(arg)) =>
            totalForRecall += 1
            totalForPrecision += 1

            if (goldArg._2.argumentType == arg._2 && isInsideSpan(arg._1.left, goldArg._1)) {
              correct += 1
            } else {
              allCorrect = false
            }
        }
      }

      if (allCorrect) sentCorrect += 1
    }

    ((correct.toDouble / totalForPrecision, correct.toDouble / totalForRecall), sentCorrect.toDouble / sentTotal)
  }

  def calculateResults(assignmentWithGold: Array[(Seq[Option[(Span, String)]], Seq[Option[(Span, Argument)]], Sentence)]) = {
    var total = 0
    var correct = 0

    var sentCorrect = 0

    var correctStructs = 0.0
    var totalStructs = 0.0

    for (sentence <- assignmentWithGold) {
      var allCorrect = true

      val structs = mutable.Map[Int, Boolean]() ++ sentence._2.flatten.map(_._1.left).toSet.toSeq.map((x:Int) => x -> true)

      for (instance <- sentence._1.zip(sentence._2)) {
        val predicted = instance._1
        val gold = instance._2
        // TODO: very simplicistic! assumes that arguments are in right places and are correct!
        // it measures just an assignment
        (predicted, gold) match {
          case (Some(p), Some(g)) =>
            total += 1
            if (isInsideSpan(p._1.left, g._1)) {

              correct += 1
            } else {
              structs(g._1.left) = false
              allCorrect = false
            }
          case (_, Some(g)) =>
            structs(g._1.left) = false
            allCorrect = false
            total += 1
          case _ => ;
        }
      }
      if (allCorrect) sentCorrect += 1

      correctStructs += structs.count(_._2)
      totalStructs += structs.size
    }

    (correct.toDouble / total, correctStructs.toDouble / totalStructs, sentCorrect.toDouble / assignmentWithGold.size, total)
  }

  def htmlReport(sentence: Sentence, words: Seq[Word], args: Seq[String], gold: Seq[Option[(Span, Argument)]],
                 answers: collection.Map[String, Seq[Option[(Span, String)]]], semHeads: Seq[Option[Int]]) : String = {
    val colors = Seq("#CC66FF", "#CC9900", "#0033CC"," #CCFF00", "#30D5C8")
    val predicates = gold.flatMap(_.map(_._1)).distinct.zipWithIndex.map(x => (x._1, colors(x._2 % colors.size)))

    def findColor(span: Span) : Option[String] = {
      for (predicateColor <- predicates) {
        val predSpan = predicateColor._1
        val predColor = predicateColor._2

        if (predSpan.left <= span.left && predSpan.right >= span.right)
          return Some(predColor)
      }
      return None
    }

    val goldArgs = gold.map(_.map(x => (x._1, x._2.argumentType)))

    var wrong = false
    val rows = for (assignment <- answers + ("gold" -> goldArgs)) yield {
      val wordColors: IndexedSeq[(String, String)] = for (i <- 0 until words.size) yield {
        val goldArgument = gold(i)
        val predictedArgument = assignment._2(i)
        val goldColor = goldArgument.flatMap(a => findColor(a._1)).getOrElse("black")
        val predictedColor = predictedArgument.flatMap(a => findColor(a._1)).getOrElse("black")

        (goldColor, predictedColor)
      }


      val cells = for (i <- 0 until words.size) yield {
        val word = words(i)
        val colors = wordColors(i)
        val predColor = findColor(Span(i, i+1)).getOrElse("black")
        val semHead = semHeads.lift(i).flatMap(identity _)

        val printColor = (colors._2, predColor) match {
          case ("black", x) => x
          case (y, _) => y
        }

        val background = (colors) match {
          case ("black", "black") => "white"
          case (gold, pred) if gold == pred => "#ACE1AF"
          case _ => wrong=true; "#CC6666"
        }

        val arg = if (args(i) == "_") "" else args(i)

        val semHeadNotifier = semHead.map {
          case syntHeadIndex => s"<sup><small>sem:${words(syntHeadIndex).orth}</small></sup>"
        }.getOrElse("")

        s"<td style='background-color: $background'><span style='color: ${printColor}'>${word.orth}</span><small>$arg</small>$semHeadNotifier</td>"
      }

      cells.mkString(s"<tr><th>${assignment._1}: </th>", "", "</tr>")
    }

    if (wrong) rows.mkString("<p><table>", "\n", "</table></p>")
    else ""
  }


  def isInsideSpan(index: Int, span: Span) = (span.left <= index && index <= span.right)
}

object ArgumentWiseNearestPredicate extends PredicateAssigner {
  override def assign(words: Seq[Word], arguments: Seq[String]): Seq[Option[(Span, String)]] = {
    val predicates = findPredicates(words, arguments)
    val locations = mutable.ArrayBuffer[Int]()
    val indexToShort = mutable.Map[Int, Int]()
    val shortArguments = mutable.ArrayBuffer[String]()
    val shortWords = mutable.ArrayBuffer[Word]()
    for (i <- 0 until arguments.size) yield {
      if (arguments(i) != "_" || isPredicate(words(i))) {
        indexToShort += (i -> locations.size)
        locations += i
        shortArguments += arguments(i)
        shortWords += words(i)
      }
    }

    val shortAssignment = NearestPredicateAssigner.assign(shortWords, shortArguments)

    var currentLocationsIndex = 0
    val result = for (i <- 0 until arguments.size) yield {
      if (currentLocationsIndex < locations.size && locations(currentLocationsIndex) == i) {
        currentLocationsIndex += 1
        shortAssignment(currentLocationsIndex - 1) match {
          case Some((Span(from, to), arg)) if from != -1 => Some((Span(locations(from), locations(from)+1), arg))
          case Some(other) => Some(other)
          case None => None
        }

      } else {
        None
      }
    }
    result

  }
}

object NearestPredicateAssigner extends PredicateAssigner {
  override def assign(words: Seq[Word], arguments: Seq[String]) : Seq[Option[(Span, String)]] = {
    val predicates = findPredicates(words, arguments)
    val result = for ((argument, i) <- arguments.zipWithIndex) yield {
      if (argument != "_") {
        val closestPredicate = findClosestPredicate(i, predicates)
        Some((closestPredicate, argument))
      } else {
        None
      }
    }

    result
  }


  def findClosestPredicate(wordIndex: Int, predicates: Seq[Span]) = {
    val predicateDistances = predicates.iterator.map { predicate =>
      val distance = calculateDistance(wordIndex, predicate)
      (distance, predicate)
    }.toList // todo: remove toList
    //println("distances", wordIndex, predicateDistances)
    if (predicateDistances.isEmpty) Span(-1,-1)
    else predicateDistances.minBy(_._1)._2
  }

  def calculateDistance(index: Int, span: Span) = {
    import Math.{ abs, min }
    val left = abs(index - span.left)
    left
  }
}

object RulesAssigner {
  val allRules = List[List[Rule]](
    List(NearestWord),
    List(NearestByArgument),
    List(NearestByWordWithSkipping, NearestWord),
    List(NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(new WalentySupportedNearest(false), NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestInsideTheSubsentence, NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SieArgument, SentPRule, NearestInsideTheSubsentence, NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    // This two has the same set of rules but different orders of WalentySupportedNearest(false)
    List(SieArgument, SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), new WalentySupportedNearest(false), NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SieArgument, SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord),

    List[Rule](SieArgument, SentPRule, NearestInsideTheSubsentence, NearestByWordWithSkipping, NearestWord),
    List(NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord),

    // no sie
    List(SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), new WalentySupportedNearest(false), NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    // as best one but sie in different place!
    List(SentPRule, NearestInsideTheSubsentence, SieArgument, new WalentySupportedNearest(true), new WalentySupportedNearest(false), NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestInsideTheSubsentence, SieArgument, new WalentySupportedNearest(true), NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), SieArgument, new WalentySupportedNearest(false), NearestByArgumentWithSkipping, NearestByArgument, NearestByWordWithSkipping, NearestWord),
    List(SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), SieArgument, NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord)

  )
}

case class RulesAssigner(n: Int) extends AbstractRulebasedPredicateAssigner {

  override val rules = RulesAssigner.allRules(n)
}

object WordRulebasedPredicateAssigned extends AbstractRulebasedPredicateAssigner {
  override val rules = List[Rule](SieArgument, SentPRule, NearestInsideTheSubsentence, NearestByWordWithSkipping, NearestWord)
}

object RulebasedPredicateAssigner extends AbstractRulebasedPredicateAssigner {
  override val rules = List[Rule](SieArgument, SentPRule, NearestInsideTheSubsentence, new WalentySupportedNearest(true), NearestByArgumentWithSkipping, new WalentySupportedNearest(false), NearestByArgument, NearestByWordWithSkipping, NearestWord)
}

abstract class AbstractRulebasedPredicateAssigner extends PredicateAssigner {

  var keepNotConnected: Boolean = false
  val rules: List[Rule]

  override def assign(words: Seq[Word], arguments: Seq[String]) : Seq[Option[(Span, String)]] = {
    val infPredicates = findPredicates(words, arguments, true).toIndexedSeq
    val noInfPredicates = findPredicates(words, arguments, false).toIndexedSeq

    for ((arg, i) <- arguments.zipWithIndex) yield {
      if (arg == "_") {
        None
      } else {
        val predicates = if (shouldUseInfPredicates(arg)) infPredicates else noInfPredicates
        val span = rules.foldLeft[Option[Span]](None) {
          case (currentSpan, rule) => rule.apply(words, arguments, predicates, i, currentSpan)
        }
        if (!keepNotConnected) span.map(_ -> arg)
        else span.orElse(Some(Span(-1, -1))).map(_ -> arg)
      }
    }
  }

  def shouldUseInfPredicates(arg: String) = {
    !Set("subj").exists(arg.startsWith _)
  }
}

abstract class Rule extends ((Seq[Word], Seq[String], Seq[Span], Int, Option[Span]) => Option[Span]) with Serializable

class WalentySupportedNearest (onlyInf: Boolean) extends Rule {

  def findDistanceToTheLeft(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int, depth: Int = 1) : Option[(Span, Int)] = {
    def matchWalenty(p: (Span, Int)) = {
      argumentMatchesWalenty(arguments(pos), words(p._1.left))
    }
    def isInfP(index: Int) = words(index).ctag.contains("inf:")

    val left = NearestByArgumentWithSkipping.findDistanceToTheLeft(predicates, words, arguments, pos)
    left.filter(matchWalenty _).orElse {
      if (depth < 2 && left.isDefined && isInfP(left.get._1.left)) findDistanceToTheLeft(predicates.filterNot(_.left == left.get._1.left), words, arguments, pos, depth+1)
      else None
    }
  }

  def argumentMatchesWalenty(arg: String, predicate: Word) = {
    val lemma = predicate.base
    val aspect = findAspect(predicate.ctag)
    FlatWalentyInstnce.checkArgument(lemma, aspect, arg)
  }

  def findAspect(ctag: String) = {
    if (ctag.contains("imperf")) "imperf"
    else if (ctag.contains("perf")) "perf"
    else "_"
  }

  def findDistanceToTheRight(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int, depth: Int = 1) : Option[(Span, Int)] = {
    def matchWalenty(p: (Span, Int)) = {
      argumentMatchesWalenty(arguments(pos), words(p._1.left))
    }
    def isInfP(index: Int) = words(index).ctag.contains("inf:")

    val right = NearestByArgumentWithSkipping.findDistanceToTheRight(predicates, words, arguments, pos)
    right.filter(matchWalenty _).orElse {
      if (depth < 2 && right.isDefined && isInfP(right.get._1.left)) findDistanceToTheRight(predicates.filterNot(_.left == right.get._1.left), words, arguments, pos, depth+1)
      else None
    }
  }

  def walentyHasPredicate(word: Word) = {
    val lemma = word.base
    val aspect = findAspect(word.ctag)
    FlatWalentyInstnce.hasPredicate(lemma, aspect)
  }

  def shouldRun(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Boolean = {
    val left = NearestByArgumentWithSkipping.findDistanceToTheLeft(predicates, words, arguments, pos)
    val right = NearestByArgumentWithSkipping.findDistanceToTheRight(predicates, words, arguments, pos)

    if (!left.isDefined) return false
    if (!right.isDefined) return false

    if (!walentyHasPredicate(words(left.get._1.left))) return false
    if (!walentyHasPredicate(words(right.get._1.left))) return false

    def isInfP(p: (Span, Int)) = {
      words(p._1.left).ctag.contains("inf:")
    }

    // use Only with infp?
    return !onlyInf || left.filter(isInfP _).orElse(right.filter(isInfP _)).isDefined
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    // TODO; check if it works! (i.e. anyNeighbouringisInfP)
    if (current.isEmpty && shouldRun(predicates, words, arguments, pos)) {
      val leftOpt = findDistanceToTheLeft(predicates, words, arguments, pos)
      val rightOpt = findDistanceToTheRight(predicates, words, arguments, pos)
      /*if (words(0).orth == "Zakradał" && words(1).orth == "em") {
         println(leftOpt, rightOpt, getNearestFrom(leftOpt, rightOpt))
      }*/
      NearestByArgumentWithSkipping.getNearestFrom(leftOpt, rightOpt)
    } else {
      current
    }
  }
}

object SieArgument extends Rule {
  val sieVerbs = Source.fromFile("sie_verbs")(Codec.UTF8).getLines().map(_.trim).toSet

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty && arguments(pos) == "sie") { // current.orElse {}
      val leftPred = NearestByArgumentWithSkipping.findDistanceToTheLeft(predicates, words, arguments, pos)
      val rightPred = NearestByArgumentWithSkipping.findDistanceToTheRight(predicates, words, arguments, pos)

      def filterOutWithWalenty(p: (Span, Int)) = {
        verbWithSie(words(p._1.left))
      }

      def maxDistance(maxDistance: Int)(p: (Span, Int)) = maxDistance >= p._2
      def potentialObjectOnTheRight(p: (Span, Int)) : Boolean = {
        for (i <- pos until p._1.left) {
          if (arguments(i) == "np(bier)")
            return false
        }
        return true
      }

      val filteredLeft = leftPred.filter(filterOutWithWalenty _).filter(maxDistance(2))
      val filteredRight = rightPred.filter(filterOutWithWalenty _).filter(maxDistance(2)).filter(potentialObjectOnTheRight _)

      def isInfp(p: (Span, Int)) = words(p._1.left).ctag.startsWith("inf:")
      val infpRightOrLeft: Option[Span] = rightPred.filter(isInfp _).orElse(leftPred).map(_._1)

      if (filteredLeft != None && filteredRight != None) {
        infpRightOrLeft // or None?
      } else
        filteredLeft.orElse(filteredRight).map(_._1).orElse(infpRightOrLeft)
    } else {
      current
    }
  }

  def verbWithSie(word: Word) = {
    sieVerbs.contains(word.base)
  }
}


object NearestWord extends Rule {
  def findDistanceToTheLeft(predicates: Seq[Span], pos: Int) = {
    // span.right is not inclusive!
    Try {
      val minSpan = predicates.filter(_.right < pos + 1).minBy(pos + 1 - _.right)
      (minSpan, pos + 1 - minSpan.right)
    }.toOption
  }

  def findDistanceToTheRight(predicates: Seq[Span], pos: Int) = {
    // span.left is inclusive!
    Try {
      val minSpan = predicates.filter(_.left > pos).minBy(_.left - pos)
      (minSpan, minSpan.left - pos)
    }.toOption
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      val leftOpt = findDistanceToTheLeft(predicates, pos)
      val rightOpt = findDistanceToTheRight(predicates, pos)
      (leftOpt, rightOpt) match {
        case (Some(left), Some(right)) =>
          if (right._2 <= left._2) Some(right._1)
          else Some(left._1)
        case (Some(left), None) => Some(left._1)
        case (None, Some(right)) => Some(right._1)
        case _ => None
      }
    } else {
      current
    }
  }
}

object SentPRule extends Rule {
  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      if (arguments(pos).toLowerCase.startsWith("sentp")) {
        val skip = NearestByArgumentWithSkipping.findSkipRightValue(words, arguments, pos)
        val result = NearestWord.findDistanceToTheLeft(predicates, pos).map(_._1)
        result.orElse(NearestByArgumentWithSkipping.findDistanceToTheRight(predicates, words, arguments, pos+skip-1).map(_._1))
      } else {
        None
      }
    } else {
      current
    }
  }
}

object NearestByArgumentWithSkipping extends Rule {
  def isPredicate(predicates: Seq[Span], pos: Int) : Boolean = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return true
    }
    return false
  }

  def whichPredicate(predicates: Seq[Span], pos: Int) : Span = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return p
    }
    throw new RuntimeException("pos not in predicates!")
  }

  def findSkipLeftValue(words: Seq[Word], arguments: Seq[String], curPos: Int) : Int = {
    if (NearestInsideTheSubsentence.isSubsentenceEndIndicator(words, arguments, curPos) && curPos > 0) {
      val subsentBeginning = NearestInsideTheSubsentence.findSubsentenceIndicatorOnTheLeft(words, arguments, curPos-1)
      return subsentBeginning.map(curPos - _ + 1).getOrElse(0)
    }
    return 0
  }

  def findSkipRightValue(words: Seq[Word], arguments: Seq[String], curPos: Int) : Int = {
    if (NearestInsideTheSubsentence.isSubsentenceIndicator(words, arguments, curPos) && curPos < words.size) {
      val subsentEnd = NearestInsideTheSubsentence.findSubsentenceEndIndicatorOnTheRight(words, arguments, curPos+1)
      return subsentEnd.map(_ - curPos + 1).getOrElse(0)
    }
    return 0
  }

  def findDistanceToTheLeft(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until pos).map(index => isPredicate(predicates, index))
    var curPos = pos - 1
    var distance = 1
    while (curPos >= 0) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      val skip = findSkipLeftValue(words, arguments, curPos)
      if (skip == 0) {
        distance += increaseDistance(curPos, arguments, words)
        curPos -= 1
      } else {
        curPos -= skip
        distance += 2
      }
    }
    return None
  }

  def findDistanceToTheRight(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until arguments.size).map(index => isPredicate(predicates, index))
    var curPos = pos + 1
    var distance = 1
    val size = arguments.size

    while (curPos < size) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      val skip = findSkipRightValue(words, arguments, curPos)
      if (skip == 0) {
        distance += increaseDistance(curPos, arguments, words)
        curPos += 1
      } else {
        curPos += skip
        distance += 2
      }
    }
    return None
  }

  def increaseDistance(curPos: Int, args: Seq[String], words: Seq[Word]) = {
    val arg = args(curPos)
    val word = words(curPos)

    val pos = word.ctag.split(":")(0)
    // TODO: possibly check if (index+1 or index+2) is a verb in that case penalty should be higher than two (4-
    val predicateNearby = (curPos + 1 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+1))) || (curPos + 2 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+2)))
    val predicatePenalty = if (predicateNearby) 4 else 0
    if (Seq("i", "oraz", "lub", "albo", "bądź").contains(word.base)) (2+predicatePenalty)
    else if (pos == "interp") 2 + predicatePenalty
    else if (arg != "_") 1
    else 0
  }

  def getNearestFrom(leftOpt: Option[(Span, Int)], rightOpt: Option[(Span, Int)]) = {
    (leftOpt, rightOpt) match {
      case (Some(left), Some(right)) =>
        if (right._2 < left._2) Some(right._1)
        else if (right._2 > left._2) Some(left._1)
        else None
      case (Some(left), None) => Some(left._1)
      case (None, Some(right)) => Some(right._1)
      case _ => None
    }
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      val leftOpt = findDistanceToTheLeft(predicates, words, arguments, pos)
      val rightOpt = findDistanceToTheRight(predicates, words, arguments, pos)
      getNearestFrom(leftOpt, rightOpt)
    } else {
      current
    }
  }
}


object NearestInsideTheSubsentence extends Rule {
  val SUBCONJ = "a, ale, bo, ponieważ, gdyż, lecz, jednak, jednakże, zaś, wszakże, owszem, natomiast, tylko, tylko że, jedynie, przecież, raczej, tymczasem, za, więc, dlatego, toteż, to, zatem, skutkiem, wskutek, czyli, czy, gdy, gdyby, choć, zanim, aby, żeby, kiedy, nim, jeśli, jeżeli, jaki, jaka, jakie, który, które, która, jak".split(", ").toSet

  def isSubsentenceIndicator(words: Seq[Word], arguments: Seq[String], index: Int) : Boolean = {
    val pos = words(index).ctag.split(":")(0)
    val base = words(index).base
    if (arguments(index).startsWith("sentp")) return true
    if (index == 0 && SUBCONJ.contains(base)) return true
    if (index > 0) {
      val prevPos = words(index-1).ctag.split(":")(0)
      if (SUBCONJ.contains(base) && prevPos == "interp") return true
      if (index < words.size - 1 && words(index-1).base == "," && pos == "prep" && SUBCONJ.contains(words(index+1).base)) return true
      if (base == ";") return true
    }
    return false
  }

  def isSubsentenceEndIndicator(words: Seq[Word], arguments: Seq[String], index: Int) : Boolean = {
    val pos = words(index).ctag.split(":")(0)
    val base = words(index).base
      if (pos == "interp" && base != "," && base != ".") return true
    if (index > 0 && index < words.size - 1) {
      val prevPos = words(index-1).ctag.split(":")(0)
      val nextPos = words(index+1).ctag.split(":")(0)
      if (base == "," && (prevPos != "adj" || nextPos != "adj")) return true
    } else if (index > 0) {
      val prevPos = words(index-1).ctag.split(":")(0)
      if (base == "," && prevPos != "adj") return true
    }
    return false
  }

    def findSubsentenceIndicatorOnTheLeft(words: Seq[Word], arguments: Seq[String], pos: Int) : Option[Int] = {
    var cur = pos

    while (cur >= 0) {
      if (isSubsentenceIndicator(words, arguments, cur)) return Some(cur)
      cur -= 1
    }
    return None
  }

  def findSubsentenceEndIndicatorOnTheRight(words: Seq[Word], arguments: Seq[String], pos: Int) : Option[Int] = {
    var cur = pos
    val size = words.size

    while (cur < size) {
      if (isSubsentenceEndIndicator(words, arguments, cur)) return Some(cur)
      cur += 1
    }
    return None
  }

  def findSubsentenceSpan(words: Seq[Word], arguments: Seq[String], pos: Int): Option[(Int, Int)] = {
    val left = findSubsentenceIndicatorOnTheLeft(words, arguments, pos)
    val right = findSubsentenceEndIndicatorOnTheRight(words, arguments, pos)

    left match {
      case Some(left) => Some((left, right.getOrElse(words.size-1)))
      case None => None
    }
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      val span = findSubsentenceSpan(words, arguments, pos)
      span match {
        case Some((leftBound,rightBound)) =>
          val nearestLeft = NearestByArgumentWithSkipping.findDistanceToTheLeft(predicates, words, arguments, pos)
          val nearestRight = NearestByArgumentWithSkipping.findDistanceToTheRight(predicates, words, arguments, pos)
          val filteredLeft = nearestLeft.filter(_._1.right - 1 > leftBound)
          val filteredRight = nearestRight.filter(_._1.left < rightBound)

          NearestByArgumentWithSkipping.getNearestFrom(filteredLeft, filteredRight)
        case None => None
      }
    } else {
      current
    }
  }
}

object NearestByArgument extends Rule {
  def isPredicate(predicates: Seq[Span], pos: Int) : Boolean = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return true
    }
    return false
  }

  def whichPredicate(predicates: Seq[Span], pos: Int) : Span = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return p
    }
    throw new RuntimeException("pos not in predicates!")
  }

  def findDistanceToTheLeft(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until pos).map(index => isPredicate(predicates, index))
    var curPos = pos - 1
    var distance = 1
    while (curPos >= 0) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      distance += increaseDistance(curPos, arguments, words)
      curPos -= 1
    }
    return None
  }

  def findDistanceToTheRight(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until arguments.size).map(index => isPredicate(predicates, index))
    var curPos = pos + 1
    var distance = 1
    val size = arguments.size

    while (curPos < size) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      distance += increaseDistance(curPos, arguments, words)
      curPos += 1
    }
    return None
  }

  def increaseDistance(curPos: Int, args: Seq[String], words: Seq[Word]) = {
    val arg = args(curPos)
    val word = words(curPos)

    val pos = word.ctag.split(":")(0)
    // TODO: possibly check if (index+1 or index+2) is a verb in that case penalty should be higher than two (4-
    val predicateNearby = (curPos + 1 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+1))) || (curPos + 2 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+2)))
    val predicatePenalty = if (predicateNearby) 4 else 0
    if (Seq("i", "oraz", "lub", "albo", "bądź").contains(word.base)) (2+predicatePenalty)
    else if (pos == "interp") 2 + predicatePenalty
    else if (arg != "_") 1
    else 0
  }

  def getNearestFrom(leftOpt: Option[(Span, Int)], rightOpt: Option[(Span, Int)]) = {
    (leftOpt, rightOpt) match {
      case (Some(left), Some(right)) =>
        if (right._2 < left._2) Some(right._1)
        else if (right._2 > left._2) Some(left._1)
        else None
      case (Some(left), None) => Some(left._1)
      case (None, Some(right)) => Some(right._1)
      case _ => None
    }
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      val leftOpt = findDistanceToTheLeft(predicates, words, arguments, pos)
      val rightOpt = findDistanceToTheRight(predicates, words, arguments, pos)
      getNearestFrom(leftOpt, rightOpt)
    } else {
      current
    }
  }
}

object NearestByWordWithSkipping extends Rule {
  def isPredicate(predicates: Seq[Span], pos: Int) : Boolean = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return true
    }
    return false
  }

  def whichPredicate(predicates: Seq[Span], pos: Int) : Span = {
    for (p <- predicates) {
      if (p.left <= pos && pos < p.right) return p
    }
    throw new RuntimeException("pos not in predicates!")
  }

  def findSkipLeftValue(words: Seq[Word], arguments: Seq[String], curPos: Int) : Int = {
    if (NearestInsideTheSubsentence.isSubsentenceEndIndicator(words, arguments, curPos) && curPos > 0) {
      val subsentBeginning = NearestInsideTheSubsentence.findSubsentenceIndicatorOnTheLeft(words, arguments, curPos-1)
      return subsentBeginning.map(curPos - _ + 1).getOrElse(0)
    }
    return 0
  }

  def findSkipRightValue(words: Seq[Word], arguments: Seq[String], curPos: Int) : Int = {
    if (NearestInsideTheSubsentence.isSubsentenceIndicator(words, arguments, curPos) && curPos < words.size) {
      val subsentEnd = NearestInsideTheSubsentence.findSubsentenceEndIndicatorOnTheRight(words, arguments, curPos+1)
      return subsentEnd.map(_ - curPos + 1).getOrElse(0)
    }
    return 0
  }

  def findDistanceToTheLeft(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until pos).map(index => isPredicate(predicates, index))
    var curPos = pos - 1
    var distance = 1
    while (curPos >= 0) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      val skip = findSkipLeftValue(words, arguments, curPos)
      if (skip == 0) {
        distance += increaseDistance(curPos, arguments, words)
        curPos -= 1
      } else {
        curPos -= skip
        distance += 2
      }
    }
    return None
  }

  def findDistanceToTheRight(predicates: Seq[Span], words: Seq[Word], arguments: Seq[String], pos: Int) : Option[(Span, Int)] = {
    val preds = (0 until arguments.size).map(index => isPredicate(predicates, index))
    var curPos = pos + 1
    var distance = 1
    val size = arguments.size

    while (curPos < size) {
      if (preds(curPos)) return Some((whichPredicate(predicates, curPos), distance))
      val skip = findSkipRightValue(words, arguments, curPos)
      if (skip == 0) {
        distance += increaseDistance(curPos, arguments, words)
        curPos += 1
      } else {
        curPos += skip
        distance += 2
      }
    }
    return None
  }

  def increaseDistance(curPos: Int, args: Seq[String], words: Seq[Word]) = {
    val arg = args(curPos)
    val word = words(curPos)

    val pos = word.ctag.split(":")(0)
    // TODO: possibly check if (index+1 or index+2) is a verb in that case penalty should be higher than two (4-
    val predicateNearby = (curPos + 1 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+1))) || (curPos + 2 < words.size && RulebasedPredicateAssigner.isPredicate(words(curPos+2)))
    val predicatePenalty = if (predicateNearby) 4 else 0
    if (Seq("i", "oraz", "lub", "albo", "bądź").contains(word.base)) (2+predicatePenalty)
    else if (pos == "interp") 2+predicatePenalty
    else 1
  }

  def getNearestFrom(leftOpt: Option[(Span, Int)], rightOpt: Option[(Span, Int)]) = {
    (leftOpt, rightOpt) match {
      case (Some(left), Some(right)) =>
        if (right._2 < left._2) Some(right._1)
        else if (right._2 > left._2) Some(left._1)
        else None
      case (Some(left), None) => Some(left._1)
      case (None, Some(right)) => Some(right._1)
      case _ => None
    }
  }

  def apply(words: Seq[Word], arguments: Seq[String], predicates: Seq[Span], pos: Int, current: Option[Span]) = {
    if (current.isEmpty) {
      val leftOpt = findDistanceToTheLeft(predicates, words, arguments, pos)
      val rightOpt = findDistanceToTheRight(predicates, words, arguments, pos)
      getNearestFrom(leftOpt, rightOpt)
    } else {
      current
    }
  }
}