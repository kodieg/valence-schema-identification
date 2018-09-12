package kodie.phd.assigner

import kodie.phd.skladnica.types.{Span, Word}
import scala.annotation.tailrec
import kodie.phd.skladnica.SkladnicaConstants

import scala.util.Try


object SemanticHeadAssigner {
  def assign(words: Seq[Word], args: Seq[Option[(Span, String)]]) : Seq[Option[Int]] = {
    val arguments = args.map(_.map(_._2).getOrElse("_"))

    assignToArgs(words, arguments)
  }

  def assignToArgs(words: Seq[Word], arguments: Seq[String]) : Seq[Option[Int]] = {

    val predicates = ArgumentWiseNearestPredicate.findPredicates(words, arguments)

    def isPredicate(current: Int) = {
      predicates.exists(span => span.left <= current && current < span.right)
    }

    def shouldStop(current: Int, allowPredicate: Boolean) = {
      current < 0 || current >= words.size || arguments(current) != "_" || ((!allowPredicate) && isPredicate(current))
    }

    @tailrec def findContext(current: Int, step: Int, allowPredicate: Boolean = false): Int = {
      if (shouldStop(current, allowPredicate)) current - step
      else findContext(current + step, step, allowPredicate)
    }

    val onlyArgs = arguments

    arguments.zipWithIndex.map {
      case (arg, index) if arg != "_" =>
          val rightEnd =
            if (arg.startsWith("sentp")) {
              NearestInsideTheSubsentence.findSubsentenceEndIndicatorOnTheRight(words, onlyArgs, index+1).getOrElse(words.size - 1)
            } else findContext(index+1, 1)
          val rightContext = (index + 1, rightEnd)
          val leftContext = (index - 1, findContext(index-1, -1))

          val semhead = findSemanticArgumentHead(words, arg, index, rightContext, 1, leftContext) orElse findSemanticArgumentHead(words, arg, index, leftContext, -1, rightContext) getOrElse index
          Some(semhead)
      case _ => None
      }
  }

  def findSemanticArgumentHead(words: Seq[Word], argumentType: String, index: Int, searchSpan: (Int, Int), direction: Int, secondContext: (Int, Int)): Option[Int] = {
    def prepCase(arg: String) : String = {
      val end = arg.indexOf(')')
      val comma = arg.indexOf(',')
      // This replaceAll resolves some bug (",dop" as case???)
      Try(SkladnicaConstants.CASES.getOrElse(arg.substring(comma+1, end).replaceAll(",", ""), "unknown")).getOrElse("unknown")
    }
    def npCase(arg: String): String = {
      val end = arg.indexOf(')')
      val begin = arg.indexOf('(')
      Try(SkladnicaConstants.CASES.getOrElse(arg.substring(begin+1, end).replace("}", ""), "unknown")).getOrElse("unknown")
    }

    argumentType match {
          case arg if arg.startsWith("prepnp") && direction == 1 =>
            val case_ = prepCase(arg)
            locateNearbyWithCase(words, searchSpan, direction, "noun", case_) orElse (locateNearbyNotGen(words, searchSpan, direction, "noun")) orElse (locateNearby(words, searchSpan, direction, "noun"))
          case arg if arg.startsWith("prepadjp") && direction == 1 =>
            val case_ = prepCase(arg)
            locateNearbyWithCase(words, searchSpan, direction, "adj", case_) orElse (locateNearbyNotGen(words, searchSpan, direction, "noun")) orElse (locateNearby(words, searchSpan, direction, "noun"))
          case arg if arg.startsWith("sentp") && direction == 1 =>
            locateNearby(words, searchSpan, direction, "verb")
          case arg if arg.startsWith("np") && !words(index).ctag.startsWith("subst") && !words(index).ctag.startsWith("depr") =>
            val case_ = npCase(arg)
            locateNearbyWithCase(words, searchSpan, direction, "noun", case_) orElse (locateNearbyNotGen(words, searchSpan, direction, "noun")) orElse (locateNearby(words, searchSpan, direction, "noun"))
          case arg => None
    }
  }

  def locateNearby(words: Seq[Word], searchSpan: (Int, Int), direction: Int, what: String) : Option[Int] = {
    nearbyAsStream(words, searchSpan, direction, what).headOption
  }

  def locateNearbyNotGen(words: Seq[Word], searchSpan: (Int, Int), direction: Int, what: String) : Option[Int] = {
    nearbyAsStream(words, searchSpan, direction, what).filter(index => !words(index).ctag.contains("gen")).headOption
  }


  def locateNearbyWithCase(words: Seq[Word], searchSpan: (Int, Int), direction: Int, what: String, case_ : String) : Option[Int] = {
    def isCaseOk(case_ : String, ctag: String) = ctag.contains(case_)
    nearbyAsStream(words, searchSpan, direction, what).filter(index => isCaseOk(case_, words(index).ctag)).headOption
  }

  val posMap = Map("noun" -> Seq("subst", "depr", "ppron12", "ppron3", "ger" /* TODO: find more noun pos */),
                   "adj" -> Seq("adj" /* TODO: use some impts */),
                   "verb" -> Seq("inf", "praet", "fin",  "impt", "imps", "bedzie", "pred", "winien"))
  def nearbyAsStream(words: Seq[Word], searchSpan: (Int, Int), direction: Int, what: String) : Stream[Int] = {
    def is(what: String, ctag: String) = {
      posMap.getOrElse(what, Seq()).exists(ctag.startsWith _)
    }

    (searchSpan._1 to searchSpan._2 by direction).toStream.filter(index => index >= 0 && index < words.size && is(what, words(index).ctag))
  }
}
