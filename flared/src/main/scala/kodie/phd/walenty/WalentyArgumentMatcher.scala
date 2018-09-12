package kodie.phd.walenty

import kodie.phd.skladnica.SkladnicaConstants
import kodie.phd.skladnica.types.{Span, Word}
import kodie.phd.walenty.BaseMatchers.{NominalArgument, SentArgument, PrepositionalArgument}
import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter
import kodie.phd.walenty.old.NewWalentySkladnicaBridge
import experimental.{ArgumentWithProb, Argument}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.control.NonFatal

object SentenceCache {
  val maxSize = 100
  val cache = new mutable.LinkedHashMap[Seq[Word], String]()

  def makeText(words: Seq[Word]) = {
    val s = new StringBuilder(words.map(_.orth.length + 2).sum)
    for (word <- words) {
      if (word.nps) s.append(word.orth)
      else (s.append(word.orth)).append(" ")
    }
    s.toString().trim()
  }

  def get(words: Seq[Word]) = {
    cache.keys.head
    cache.getOrElseUpdate(words, makeText(words))
  }
}

object WalentyArgumentMatcher {

  def matches(text: String, skip: Int, index: Int, words: Seq[Word], semHead: Option[Word], syntHead: Option[Word], skladnicaType: String, walentyArgument: WalentyArgument, sArgs: Seq[ArrayBuffer[ArgumentWithProb]]): Boolean = {

    val r =  SimpleLexicalizedMatcher.matches(text, skip, index, words, syntHead, semHead, skladnicaType, walentyArgument, sArgs) ||
      (BaseArgumentsMatcher.matches(skladnicaType, walentyArgument, syntHead.map(_.base).getOrElse("")) && !PrepNcpWithAnyConj.matches(index, words, skladnicaType, walentyArgument)) ||
      XPArgumentsMatcher.matches(text, skip, words, index, syntHead.map(_.base), skladnicaType, walentyArgument,  syntHead, semHead, sArgs) ||
      NCPMatcher.matches(index, words, skladnicaType, walentyArgument)

    /*if (text.startsWith("wraz z córką anią codziennie") && walentyArgument.text.contains("dyszel")) {
      println(s"LEX $text ---> $skladnicaType   $walentyArgument")
      println(s"result simple lex ${SimpleLexicalizedMatcher.matches(text, skip, index, words, syntHead, semHead, skladnicaType, walentyArgument, sArgs)}")
      println(s"base: ${(BaseArgumentsMatcher.matches(skladnicaType, walentyArgument, syntHead.map(_.base).getOrElse("")) && !PrepNcpWithAnyConj.matches(index, words, skladnicaType, walentyArgument))}")
      println(s"xp: ${XPArgumentsMatcher.matches(text, skip, words, index, syntHead.map(_.base), skladnicaType, walentyArgument,  syntHead, semHead, sArgs)}")
      println(s"npc ${NCPMatcher.matches(index, words, skladnicaType, walentyArgument)}")
      println(s"overall result: $r")
    }*/

    //println(s"$syntHead, $semHead, $skladnicaType, ${walentyArgument.text} -- >$r")

    r
    // TODO: add here more matchers like lexicalized matcher!
  }


  def makeText(words: Seq[Word]) = {
    val s = new StringBuilder(words.map(_.orth.length + 2).sum)
    for (word <- words) {
      if (word.nps) s.append(word.orth)
      else (s.append(word.orth.toLowerCase)).append(" ")
    }
    s.toString().trim()
  }

  def matchingElements(words: Seq[Word], arguments: Seq[ArrayBuffer[ArgumentWithProb]], frame: WalentyFrame) : Seq[Option[(ArgumentWithProb, WalentyArgument)]] = {
    val text = makeText(words)
    var skip = 0
    for (((maybeArg,synHead),index) <- arguments.zip(words).zipWithIndex) yield {
      // TODO: predicate span?!
      //println(s"SKIP $text $skip/${text.length()}")
      val fixed = Stream(frame.arguments.find(walentyArgument => FixedMatcher.matches(text, skip, words, "", walentyArgument)).map(x => ArgumentWithProb("", Span(-1, -1), None, -1) -> x))
      skip += synHead.orth.length + (if (synHead.nps) 0 else 1)
      (fixed ++ maybeArg.toStream.map { arg =>
        val semHead = arg.semanticHead.map(words.apply)
        val matching = frame.arguments.find(walentyArgument => matches(text, skip, index, words, semHead, Some(synHead), arg.argumentType, walentyArgument, arguments))

        matching.map(m => arg -> m)
      }).filter(_.isDefined).map(_.get).headOption
    }
  }

}

object ReflMatcher {
  def matches(word: Word, walentyArgument: WalentyArgument) = {
     (word.orth.toLowerCase() == "siebie" && walentyArgument.realizations.exists(_ == "refl"))
  }
}

object PrepNcpWithAnyConj {


  def matches(index: Int, words: Seq[Word], skladnicaArgument: String, walentyArgument: WalentyArgument) = {
    def findWord(dir: Int, limit: Int, baseReq: Seq[String], posReq: Option[String]=None): Boolean = {
      (1 to limit).foreach { i =>
        val w = words.lift(i + index)
        w.foreach { word =>
          val br = baseReq.exists(brr => brr == word.orth.toLowerCase)
          val pr = posReq.map(prr => word.ctag.startsWith(prr) && word.base != "to").getOrElse(true)
          if (pr && br) return true
        }
      }
      return false
    }
    PrepositionalArgument.unapply(skladnicaArgument) match {
      case Some(_) => findWord(1, 3, List("tym", "tych", "tego")) && findWord(1, 4, List.empty, Some("conj"))
      case None => false
    }
  }

}

object FixedMatcher {

  object BracketAndStringAwareSplitter {
    def apply(sep: Char, string: String) = {
      val size = string.length
      var lastSep = 0
      var skipping = 0
      var inStr = false

      val buffer = ArrayBuffer[String]()

      for (i <- 0 until size) {
        val c = string.charAt(i)
        if ((c == '}' || c == ')') && skipping > 0) skipping -= 1
        else if (c == '{' || c == '(') skipping += 1
        else if (c == '\'' && inStr) skipping -= 1
        else if (c == '\'' && !inStr) skipping += 1
        else if (skipping == 0 && sep == c) {
          buffer += string.substring(lastSep, i)
          lastSep = i + 1
        }
      }
      if (lastSep != size) buffer += string.substring(lastSep, size)
      buffer.toArray
    }
  }

  val comprepnp = "comprepnp\\(([^\\)]*)\\)".r
  val fixed = "fixed\\((.*)\\)".r

  lazy val xpToFixed = {
    val realizations = new NewWalentyRealizationList()

    realizations.realizations.collect {
      case (key, values) if key.startsWith("xp") =>
        key -> values.filter(a => a.startsWith("comprep") || a.startsWith("fixed")).flatMap(find)
    }
  }

  def matches(fulltext: String, skip: Int, words: Seq[Word], skladnicaArgument: String, walentyArgument: WalentyArgument) = {
    // TODO: ta implementacja jest naiwna (szuka gdziekolwiek fixed strings, a mogło by sprawdzać czy argument w tym miejscu zostałby przypisany do odpowiedniego predykatu)


    val maybeFixed: Seq[String] = walentyArgument.realizations.flatMap(find _)
    //val text = makeText(words.drop(index))
    val text = fulltext.substring(skip)

    maybeFixed.exists {
      fixed =>
        text.startsWith(fixed.toLowerCase)
    }
  }



  def find(skladnicaArgument: String): Seq[String] = skladnicaArgument match {
    case comprepnp(words) => Seq(words.trim())
    case fixed(inside) =>
      val words = BracketAndStringAwareSplitter(',', inside).last.trim
      Seq(words.substring(1, words.length - 1))
    case x if x.startsWith("xp") && xpToFixed != null =>
      xpToFixed.getOrElse(x, Seq.empty)
      /*XPArgumentsMatcher.realizations.realizations.getOrElse(x, Seq.empty).flatMap {
        case a if a.startsWith("comprepnp") || a.startsWith("fixed") => find(a)
        case _ => Seq.empty
      }.toSet.toSeq*/
    case _ => Seq.empty
  }
}

object BaseArgumentsMatcher {
  def matches(skladnicaArgument: String, walentyArgument: WalentyArgument, base: String) = {
    import BaseMatchers._
    val sentArg = new SentArgumentHelper(base)
    skladnicaArgument match {
      case "subj" => walentyArgument.modifiers.contains("subj") && walentyArgument.realizations.exists(_.startsWith("np("))
      case "sie" => walentyArgument.realizations.contains("refl")
      case "refl" => walentyArgument.realizations.contains("refl")  // "refl" is not in skladnica, but may be introduced by SiebieHeuristic!
      case "nonch" =>  walentyArgument.realizations.contains("nonch")
      case NominalArgument(p) => p.filter(s => s != "np(str)" || !walentyArgument.modifiers.contains("subj")).exists(walentyArgument.realizations.contains _)
      case AdjectivalArgument(p) => p.exists(walentyArgument.realizations.contains _)
      case PrepositionalArgument(p) =>  p.exists(walentyArgument.realizations.contains _)
      case sentArg(p) => p.exists(walentyArgument.realizations.contains _)
      case InfArgument(p) => p.exists(walentyArgument.realizations.contains _)
      // problem with xp(*) - that are realized by prepositional
      case AdvpArgument(p) => p.exists(walentyArgument.realizations.contains _)
      // argument {or} -- mowa zależna
      // preplexnp?
      // prepncp -- czy w formalizmie składnicy w ogóle jest opowienik?
      case _ => false
    }
  }

}

object NCPMatcher {
  val prepncp = "prepncp\\(([^,]*),([^,]*),([^,]*)\\)".r
  val ncp = "ncp\\(([^,]*),([^,]*)\\)".r
  val QUESTION_WORDS = Set("kto", "co", "który", "która", "które", "którzy", "jaki", "jaka", "jakie" , "jacy", "kiedy", "gdy", "gdzie", "jak", "którędy", "skąd", "dokąd", "ile", "kim", "komu", "kogo", "czego", "czy", "czym", "czego", "dlaczego", "jak")
  // TODO: FIX question words! not all above are question words
  // TODO: in new version rel words are missing

  def ncpRealization(realization: String) = realization.startsWith("prepncp") || realization.startsWith("ncp")
  def matches(index: Int, words: Seq[Word], skladnicaArgument: String, argument: WalentyArgument): Boolean = {
    def findWord(base: String, index: Int, direction: Int, distance: Int) : Option[Word] = {
      for (i <- (index + direction) to (index + direction * distance) by direction) {
        if (i < words.size && i >= 0) {
          if (base == "int")
            QUESTION_WORDS.contains(words(i).base)
          else if (words(i).base == base) return Some(words(i))
        }
      }
      return None
    }

    def findNounByCase(theCase: String, index: Int, direction: Int, distance: Int, base: Option[String] = None) : Option[Word] = {
      def baseMatches(word: Word) = {
        base.map(s => s == word.base).getOrElse(true)
      }
      for (i <- (index + direction) to (index + direction * distance) by direction) {
        if (i < words.size && i >= 0) {
          if ((words(i).ctag.startsWith(s"subst") || words(i).ctag.startsWith("depr")) && words(i).ctag.contains(theCase) && baseMatches(words(i))) return Some(words(i))
        }
      }
      return None
    }

    argument.realizations.filter(ncpRealization).exists { realization =>
      realization match {
        case prepncp(prep, case_, conj) =>
          val isPrepArgument = PrepositionalArgument.unapply(skladnicaArgument).getOrElse(Seq.empty).contains(s"prepnp($prep,$case_)")
          val isSentArgument = SentArgument.unapply(skladnicaArgument, "?").getOrElse(Seq.empty).contains(s"cp($conj)")

          if (isPrepArgument) {
            findWord(conj, index, 1, 4).isDefined && findNounByCase(case_, index, 1, 3, Some("to")).isDefined
          } else if (isSentArgument) {
            findWord(prep, index, -1, 4).filter(_.ctag.contains(case_)).isDefined && findNounByCase(case_, index, -1, 3, Some("to")).isDefined
          } else {
            false
          }
        case ncp(case_, conj) =>
          val isNominalArgument = NominalArgument.unapply(skladnicaArgument).getOrElse(Seq.empty).contains(s"np($case_)")
          val isSentArgument = SentArgument.unapply(skladnicaArgument, "?").getOrElse(Seq.empty).contains(s"cp($conj)")

          if (isNominalArgument) {
            findWord(conj, index, 1, 4).isDefined
          } else if (isSentArgument) {
            // TODO: in case of obj -> str == acc,gen, in subj --> nom, do not use all, stop at predicate?
            val possibleCases = if (case_ == "str" || case_ == "part") Seq("nom", "acc", "gen") else Seq(case_)
            possibleCases.exists(theCase => findNounByCase(theCase, index, -1, 4, Some("to")).isDefined)
          } else {
            false
          }
        case _ => false
      }
    }
  }
}

object XPArgumentsMatcher {
  val realizations = new NewWalentyRealizationList()
  // realization might be comprepnp... or lex.... :/ this function does not catch it!

  def matches(text: String, _skip: Int, words: Seq[Word], index: Int, argumentSyntacticHeadBase: Option[String], skladnicaArgument: String, walentyArgument0: WalentyArgument,  syntHead: Option[Word], semHead: Option[Word],  sArgs: Seq[ArrayBuffer[ArgumentWithProb]]) = {
    val walentyArgument = walentyArgument0.copy(realizations = walentyArgument0.realizations.filter(_.startsWith("xp")))
    val translatedTypes = NewWalentySkladnicaBridge.translate(skladnicaArgument) // TODO: this uses old API!
    val realizationsByType = translatedTypes.flatMap { translatedType => realizations.skladnicaPossibilities.getOrElse(translatedType, Seq()) }.toSet
    val realizationsByHead = if (argumentSyntacticHeadBase.isDefined) {
      realizations.skladnicaPossibilities.getOrElse(argumentSyntacticHeadBase.get, Seq())
    } else {
      Seq()
    }

    val opts = walentyArgument.realizations.filter(_.startsWith("xp")).flatMap { wal =>
      realizations.realizations.getOrElse(wal, Seq.empty)
    }.toSet

    def byOpts = opts.exists {
      /*case x if x.startsWith("comprepnp") || x.startsWith("fixed") =>
        var skip = 0
        try {
          words.exists { w =>
            val r = FixedMatcher.matches(text, skip, words, skladnicaArgument, walentyArgument)
            skip += w.orth.length + (if (w.nps) 0 else 1)
            if (r) println("YEAH YEAH YEAH YEAH")
            r
          }
        } catch {
          case NonFatal(e) =>
            println(s"KKKKKKK $e, $skip, ${text.length}, $skladnicaArgument, $walentyArgument, $text, $words")
            false
        }
*/
      case x if x.startsWith("lex") =>
        SimpleLexicalizedMatcher.matches(text, _skip, index, words,  syntHead, semHead, skladnicaArgument, walentyArgument, sArgs)
      case _ => false
    }

    val translTypesSet = translatedTypes.toSet

    val possibleRealizations = realizationsByType ++ realizationsByHead

    val realizationsByHeadSet = realizationsByHead.toSet

    //println(s"For: ${skladnicaArgument} :${argumentSyntacticHeadBase} realizations: $possibleRealizations")
//    /println(s"Walenty realizations: ${walentyArgument.realizations.toSet}")

    val walentyRealizations = walentyArgument.realizations.map { r =>
      if (r.contains("[")) {
        val lbr = r.indexOf('[')
        val rbr = r.indexOf(']')
        val newR = r.substring(0, lbr) + r.substring(rbr + 1)
        val opts = r.substring(lbr + 1, rbr).split(';')
        (r, Some(opts))
      } else {
        (r, None)
      }
    }.toSet
    val simple = possibleRealizations.exists(r => walentyRealizations.contains((r, None)))
    val withRestrictions = walentyRealizations.exists { case (r, restrictions) =>
      restrictions match {
        case Some(rest) =>
          val byhead = rest.exists(_.startsWith("advp")) && realizationsByHeadSet.contains(r)
          byhead || rest.toSet.intersect(translTypesSet).nonEmpty
        case None => false
      }

    }

    simple || withRestrictions || byOpts
  }
}

object SimpleLexicalizedMatcher {
  def matches(text: String, skip: Int, index: Int, words: Seq[Word], argumentSyntacticHeadBase: Option[Word], argumentSemanticHeadBase: Option[Word], skladnicaArgument: String, walentyArgument: WalentyArgument, sArgs: Seq[ArrayBuffer[ArgumentWithProb]]): Boolean = {
    def checkWord(headOption: Option[Word], baseType: String, reqNum: String, reqHead: String) = {
      headOption match {
        case None => false
        case Some(head) =>
          // Use empty seq as modifiers!
          val newType = WalentyArgument(walentyArgument.text, Seq(), Seq(baseType))
          // Checking orth, as sometimes base form is not valid (e.g. manowce vs manowiec)
          /*if (head.base == "sumienie" || reqHead == "sumienie") {
            println (reqHead, head.base, newType, skladnicaArgument,  BaseArgumentsMatcher.matches(skladnicaArgument, newType), (reqHead == head.base || reqHead == head.orth) && BaseArgumentsMatcher.matches(skladnicaArgument, newType))
          }*/

          lazy val typeMatches = if (walentyArgument.text == "_ign_") {
            true
          } else if (reqNum != "compar") {
            BaseArgumentsMatcher.matches(skladnicaArgument, newType, argumentSyntacticHeadBase.map(_.base).getOrElse(""))
          } else if (reqHead.equalsIgnoreCase(head.base) || reqHead.equalsIgnoreCase(head.orth)) {
            val r = Try{((index + 1) to (index+2)).filter(_ < words.size).exists { case i =>

                val newSemH = None //arg.semanticHead.map(words.apply)
                val newSynH = words(i)
                val newSkip = skip + (index to i).map { j => words(j).orth.length + (if (words(j).nps) 0 else 1)}.sum
                // Explicitly removing type --  lex(compar(jak),lex(adjp(...)) might be either prepadjp(jak,...) or sentp(pz) + adjp(...)
                // let's focus here only on matching reqHeads
                val rr = WalentyArgumentMatcher.matches(text, newSkip, i, words, newSemH, Some(newSynH), "_ign_", WalentyArgument("_ign_", Seq.empty, Seq(baseType) ), sArgs)
               // println(s"Index: $index, Matching ${newSynH.orth},_ign_ with _ign_ --> $rr")
                rr

            }}
            //println(s"Index: $index -- result: $r")
            r.toOption.getOrElse(false)
          } else { false }
          lazy val nearbyReq = (reqNum != "compar") && ((index-1) to (index+2)).exists(i => words.lift(i).exists(w => w.base == reqHead || w.orth == reqHead))
          val siebieHack = (reqHead == "siebie") && (head.ctag.startsWith("siebie"))

         // println(s"LEX $text")
         // println(s"Idx: $index, LexHeur: $reqHead == ${head.base} with type (${walentyArgument.text}) $baseType -- $skladnicaArgument ==> ($siebieHack || ${reqHead.equalsIgnoreCase(head.base)} || ${reqHead.equalsIgnoreCase(head.orth)} || $nearbyReq) && $typeMatches")
          //}
          // TODO zamiast base argument matcher pewnie  mozna normalnie WalentyArgumentMatcher (!) czy cos
          (siebieHack || reqHead.equalsIgnoreCase(head.base) || reqHead.equalsIgnoreCase(head.orth) || nearbyReq) && typeMatches // TODO: maybe check num
      }
    }

    walentyArgument.realizations.exists {  argument =>
      if (argument.startsWith("lex")) {
        parseLexArgument(argument) match {
          case None => false
          case Some((baseType, requiredNum, expectedHeads)) =>
            val baseHeads = parseExpectedHeads(expectedHeads)
            //println(s"XXXXXXXXX $baseHeads -- $baseType -- $requiredNum -- $expectedHeads -- $argumentSemanticHeadBase")
            baseHeads.exists { expectedHead =>
              checkWord(argumentSemanticHeadBase, baseType, requiredNum, expectedHead) ||
                checkWord(argumentSyntacticHeadBase, baseType, requiredNum, expectedHead)
            }
        }
      } else {
        false
      }

    }


  }

  private def parseExpectedHeads(spec: String) = {
    if ((spec.startsWith("OR") || spec.startsWith("XOR")) && spec.indexOf('(') != -1 && spec.indexOf(')') != -1) {
      spec.substring(spec.indexOf('(') + 1, spec.indexOf(')')).split(",|;").map(_.trim()).toSeq
    } else {
      Seq(spec)
    }
  }

  def parseLexArgument(argumentSpec: String) = {
    def cutSensiblePart(arg: String) = {
      if (arg.contains("[") && arg.contains("]")) {
        // xp(mod[np(str)])
        val start = arg.indexOf('[')
        val end = arg.lastIndexOf(']')
        arg.substring(start+1, end)
      } else {
        arg
      }
    }
    val start = argumentSpec.indexOf('(') + 1
    val parts = BracketAwareSplitter(',', argumentSpec.substring(start))
    if (parts.size < 3) {
      // sample: lex(compar(jako),np(nom))
      if (parts(0).startsWith("compar")) {
        val p = parts(0).substring(7,parts(0).size - 1)
        Some((parts(1), "compar", p))
      } else {
        None
      }
      // TODO parts(0) == 'adjp(mian)??? inne tez' to wtedy gdzie indziej jest reqHead
    } else if (parts(0).trim.startsWith("infp")) {
      val baseType = cutSensiblePart(parts(0).trim())
      val reqNum = ""
      val reqHead = parts(1).trim().replace("'", "")
      Some((baseType, reqNum, reqHead))
    } else if (parts(0).trim.startsWith("adjp")) {
      val baseType = cutSensiblePart(parts(0).trim)
      val reqNum = ""
      val reqHead = parts(4).trim().replace("'", "")

      Some((baseType, reqNum, reqHead))
    } else {

      val baseType = cutSensiblePart(parts(0).trim())
      val reqNum = parts(1).trim()
      val reqHead = parts(2).replace("'", "").trim()

      Some((baseType, reqNum, reqHead))
    }
  }
}


object BaseMatchers {

  def translateCase(skladnicaCase: String) = {
    SkladnicaConstants.CASES.getOrElse(skladnicaCase, "unknown")
  }

  object NominalArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("np(")) {
        val case_ = translateCase(arg.substring(3, arg.size - 1).trim())
        if (case_ == "acc" || case_ == "gen")
          Some(Seq(s"np($case_)", "np(str)", "np(part)"))
        else
          Some(Seq(s"np($case_)"))
      } else {
        None
      }
    }
  }

  object AdjectivalArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("adjp(")) {
        val case_ = translateCase(arg.substring("adjp(".size, arg.size - 1).trim())
        if (case_ == "acc" || case_ == "gen")
          Some(Seq(s"adjp($case_)", "adjp(str)", "adjp(part)"))
        else
          Some(Seq(s"adjp($case_)", "adjp(pred)"))
      } else {
        None
      }
    }
  }

  object PrepositionalArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("prepnp") || arg.startsWith("prepadjp")) {
        val args = arg.trim.substring(arg.trim.indexOf('(') + 1, arg.trim.indexOf(')')).split(",").map(_.trim)
        if (args.size != 2) {
          None
        } else {
          val translated = translateCase(args(1))
          if (translated == "unknown") None
          else {
            val pred = arg.trim.substring(0, arg.trim.indexOf('('))
            val exPo = if (args(0) == "po" && pred == "prepadjp") Seq("prepadjp(po,postp)") else Seq()
            if ((translated == "acc" || translated == "gen") || (translated == "nom" && args(0) == "jako")) {
              Some(Seq(s"$pred(${args(0)},$translated)", s"$pred(${args(0)},str)") ++ exPo)
            } else {
              Some(Seq(s"$pred(${args(0)},$translated)") ++ exPo)
            }
          }
        }
      } else {
        None
      }
    }
  }

  class SentArgumentHelper(base: String) {
    def unapply(arg: String) = SentArgument.unapply(arg, base)
  }

  object SentArgument {
    def unapply(arg: String, base: String): Option[Seq[String]] = {
      if (arg.startsWith("sentp")) {
        val conj = arg.substring(arg.indexOf('(') + 1, arg.indexOf(')'))
        // TODO: fixme -- cp($conj) & compar($conj) --> conj -> base!
        if (conj == "pz")
          Some(Seq(s"cp(int)", s"cp($base)", s"compar($base)"))
        else {
          if (conj == "żeby" || conj == "że" || conj == "iż")
            Some(Seq(s"cp($conj)", "cp(żeby2)"))
          else
            Some(Seq(s"cp($conj)"))
        }
      } else {
        None
      }
    }
  }

  object InfArgument {
    val translateAspect = Map("dk" -> "perf", "nd" -> "imperf")

    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("infp")) {
        val aspect = translateAspect.getOrElse(arg.substring(arg.indexOf('(') + 1, arg.indexOf(')')), "unknown")
        Some(Seq(s"infp($aspect)", "infp(_)"))
      } else {
        None
      }
    }
  }

  // TODO: use realizations to recover advp!
  object AdvpArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg == "advp") {
        Some(Seq(/*"xp(locat)", "xp(temp)", "xp(dur)", "xp(mod)", "xp(adl)", "xp(abl)", */"xp(mod)", "advp(misc)", "advp(pron)"))
      } else {
        None
      }
    }
  }

}
