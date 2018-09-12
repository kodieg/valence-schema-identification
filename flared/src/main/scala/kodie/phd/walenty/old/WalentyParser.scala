package kodie.phd.walenty.old

import kodie.phd.skladnica.SkladnicaConstants

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 *
 * @param labels subj,obj,controller,controllee,...
 */
case class WalentyArgument(labels: IndexedSeq[String], realizations: IndexedSeq[String]) {
  override def toString = s"${labels.mkString(",")}{${realizations.mkString(";")}}"

}

class NewWalenty(@transient source: Source) extends  Serializable {
  def this() = this(Source.fromFile("walenty_09_2014_all.txt"))

  val frames = {
    val parsedFrames = source.getLines().flatMap {
      WalentyParser.parseLine _
    }
    parsedFrames.toSeq.groupBy { frame =>
      //predicate, aspect
      (frame._1, frame._2)
    } mapValues { values =>
      // arguments
      values.map(_._3)
    } map(x => x)  // force to be serializable
  }

  def framesFor(predicate: String, aspect: String) = {
    // TODO: make sure that it is reasonable to do getOrElse instead of ++ both these sources (_, aspect)
    val framesWithMatchingAspect = frames.get((predicate, aspect))
    val framesForPredicate = framesWithMatchingAspect.orElse(frames.get((predicate, "_")))
    framesForPredicate.getOrElse(IndexedSeq())
  }

  def framesForWithAspect(predicate: String, aspect: String) = {
    def getFor(aspect: String) = frames.get(predicate, aspect).map((_, aspect))
    // TODO: make sure that it is reasonable to do getOrElse instead of ++ both these sources (_, aspect)
    val framesWithMatchingAspect = getFor(aspect)
    val framesForPredicate = framesWithMatchingAspect.orElse(getFor("_"))
    framesForPredicate.getOrElse((IndexedSeq(), ""))
  }

  def findNonEmptyMatchingFrames(predicate: String, aspect: String, skladnicaArguments: Seq[Option[String]]) = {
    val frames = framesFor(predicate, aspect)
    val matchings = frames.map { frame =>
       val matchingDebug = SimpleWalentyFrameMatcher.matchingElements(skladnicaArguments, frame)
      (frame, matchingDebug)
    }
    matchings.filter(!_._2.flatten.isEmpty).toIndexedSeq
  }

   // Not sure if this impl is sensible ...
  def findMaximalFrame(predicate: String, aspect: String, skladnicaArguments: Seq[Option[String]]) : (Option[IndexedSeq[WalentyArgument]], Seq[Option[WalentyArgument]], String) = {
    val (frames, aspectUsed) = framesForWithAspect(predicate, aspect)
    var maxFrame : Option[IndexedSeq[WalentyArgument]] = None
    var maxMatching = Set[String]()
    var maxMatchingDebug = Seq[Option[WalentyArgument]]()


    for (frame <- frames) {
      val matchingDebug = SimpleWalentyFrameMatcher.matchingElements(skladnicaArguments, frame)
      val matching = matchingDebug.zip(skladnicaArguments).collect {
        case (Some(sb), Some(sa)) => (sa.toString)
      }.toSet
      // println(s"$predicate --> $frame --> $matching --> $maxMatching")

      if (maxMatching.subsetOf(matching) && matching != maxMatching) {
        maxFrame = Some(frame)
        maxMatching = matching
        maxMatchingDebug = matchingDebug
      } else if (matching.subsetOf(maxMatching)  && matching != maxMatching) {
        // subset!
      } else if (!maxMatching.isEmpty) {
        // currentMax frame is wrong, but maybe somewhere in the future we have a superset?
        maxFrame = None
        // return (None, Seq())
      }
    }
    (maxFrame, maxMatchingDebug, aspectUsed)
  }

}


/**
 * This is very simple implementation of frame elements matching algorithm (I should extend it!)
 * Need to add "xp" args, prepncp "args" maybe "or" args
 */
object SimpleWalentyFrameMatcher {
  val realizations = new WalentyRelizationsList()

  def matchingElements(skladnicaArguments: Seq[Option[String]], frame: Seq[WalentyArgument]) = {
    val matchings = skladnicaArguments.map { maybeSkladnicaArg => maybeSkladnicaArg.flatMap { skladnicaArg =>
      val matching = frame.find(walentyArg =>
        NewWalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty(skladnicaArg, walentyArg))
      val matchingWithXP = matching.orElse {
        frame.find(walentyArg => NewWalentySkladnicaBridge.realizesXPArgument(realizations, skladnicaArg, walentyArg))
      }
      // TODO: lexnp, preplexnp, ncp, prepncp?
      // --> TODO: add... check if there is any advp/xp argument that could be realized by skladnicaArg!
      // --> for advp I need words (base lemma of argument or sem head)
      matchingWithXP
      }
    }
    matchings
  }
}

object WalentyParser {
  def parseLine(lineWithBom: String) = {
    val line = lineWithBom.replace('\ufeff', ' ').trim()
    val elements = line.split(":", 5)
    if (line.isEmpty || line.startsWith("%")) {
      None
    } else {
      // TODO: predicate may contain "sie" (need to hm... think how to handle it)
      var predicate = elements(0).trim()
      val aspect = elements(3).trim()
      val argumentsToParse = elements(4).trim()

      val arguments = parseArguments(argumentsToParse)

      Some((predicate, aspect, arguments))
    }
  }

  def parseArguments(argumentsSpecs: String) : IndexedSeq[WalentyArgument] = {
    argumentsSpecs.split('+').map(_.trim).map {
      parseArgument _
    }
  }

  def parseArgument(argumentSpec: String) = {
    val determinersEnd = argumentSpec.indexOf('{')
    val determiners = argumentSpec.substring(0, determinersEnd).split(",").map(_.trim()).filter(!_.isEmpty).toArray
    val argumentRealizationSpecs = argumentSpec.substring(determinersEnd+1, argumentSpec.length-1)
    val argumentRealizations: IndexedSeq[String] = argumentRealizationSpecs.split(";").map { arg =>
      arg.trim()
    }.toArray[String]

    WalentyArgument(determiners, argumentRealizations)
  }
}

class WalentyRelizationsList(source: Source) {
  def this() = this(Source.fromFile("realizations_20140523.txt"))


  val realizations = {
    val mapping = mutable.Map[String, Seq[String]]()
    var walentyType : Option[String] = None
    val realizations = ArrayBuffer[String]()
    source.getLines().foreach { line =>
      if (line.startsWith(" ")) {
        realizations += line.trim().split("\\[")(0).trim() // ignore relization status for now
      } else if (line.contains("-->")) {
        walentyType.foreach { walType =>
          mapping += walType -> realizations.toIndexedSeq
        }

        walentyType = Some(line.substring(0, line.indexOf("-->")))
        realizations.clear()
      }
    }
    walentyType.foreach { walType =>
      mapping += walType -> realizations.toIndexedSeq
    }

    mapping
  }

  val skladnicaPossibilities = {
    val one = realizations.toSeq.flatMap {
      case (walenty, skladnicaRealizations) =>
        skladnicaRealizations.map { realization =>

            (realization, walenty)

        }
    } groupBy( _._1 ) mapValues( _.map(_._2) )

    // Nested structures.... assuming depth eq 1
    val two = one.mapValues { poss =>
      poss.flatMap { entry =>
        Seq(one.get(entry).toSeq.flatten, Seq(entry)).flatten
      }
    }

    two
  }

}


// Almost full copy pase of WalentySkaldnicaBridge with minor mods to work with WalentyArgument
object NewWalentySkladnicaBridge {
  def tryToMatchSkladnicaAndWalenty(skladnicaArgument: String, walentyArgument: WalentyArgument) = {
    // We do not suppoer xp arguments to well

      skladnicaArgument match {
        case "subj" => walentyArgument.labels.contains("subj")
        case "sie" => walentyArgument.realizations.contains("refl")
        case NominalArgument(p) => p.filter(s => s != "np(str)" || !walentyArgument.labels.contains("subj")).exists(walentyArgument.realizations.contains _)
        case AdjectivalArgument(p) => p.exists(walentyArgument.realizations.contains _)
        case PrepositionalArgument(p) => p.exists(walentyArgument.realizations.contains _)
        case SentArgument(p) => p.exists(walentyArgument.realizations.contains _)
        case InfArgument(p) => p.exists(walentyArgument.realizations.contains _)
        // problem with xp(*) - that are realized by prepositional
        // case AdvpArgument(p) => p.exists(walentyArguments.contains _)
        // argument {or} -- mowa zależna
        // preplexnp?
        // prepncp -- czy w formalizmie składnicy w ogóle jest opowienik?
        case _ => false
      }
  }

  // From skladnica to list of Walenty counterparts
  def translate(skladnicaArgument: String): Seq[String] = {
    skladnicaArgument match {
    case "subj" => Seq() // don't really know how to handle --- however, this should not be needed
    case "sie" => Seq("refl")
    case NominalArgument(p) => p
    case AdjectivalArgument(p) => p
    case PrepositionalArgument(p) => p
    case SentArgument(p) => p
    case InfArgument(p) => p
    // problem with xp(*) - that are realized by prepositional
    // case AdvpArgument(p) => p.exists(walentyArguments.contains _)
    // argument {or} -- mowa zależna
    // preplexnp?
    // prepncp -- czy w formalizmie składnicy w ogóle jest opowienik?
    case _ => Seq()
  }
  }

  // TODO: add "word" parameter to check some raw realization (base forms)
  def realizesXPArgument(realizations: WalentyRelizationsList, skladnicaArgument: String,
                         walentyArgument: WalentyArgument) : Boolean = {
    // TODO: here is an error! skladnicaArgument has polish cases !
    // TODO: prepnp(po,miej) instead of prepnp(po,loc)! damn...

    val anyInteresting = NewWalentySkladnicaBridge.translate(skladnicaArgument).flatMap(realizations.skladnicaPossibilities.get(_))
    val args = anyInteresting.map { possibleWalentyArgs =>
      possibleWalentyArgs.exists { walentyArg =>
        walentyArgument.realizations.exists(walentyArg == _)
      }
    }
    args.exists(true == _)
  }

  def translateCase(skladnicaCase: String) = {
    SkladnicaConstants.CASES.getOrElse(skladnicaCase, "unknown")
  }

  object NominalArgument {
    def unapply(arg: String) : Option[Seq[String]] = {
      if (arg.startsWith("np(")) {
        val case_ = translateCase(arg.substring(3, arg.size - 1).trim())
        if (case_ == "acc")
          Some(Seq("np(acc)", "np(str)"))
        else
          Some(Seq(s"np($case_)"))
      } else {
        None
      }
    }
  }

  object AdjectivalArgument {
    def unapply(arg: String) : Option[Seq[String]] = {
      if (arg.startsWith("adjp(")) {
        val case_ = translateCase(arg.substring("adjp(".size, arg.size - 1).trim())
        if (case_ == "acc")
          Some(Seq("adjp(acc)", "adjp(str)"))
        else
          Some(Seq(s"adjp($case_)"))
      } else {
        None
      }
    }
  }

  object PrepositionalArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("prepnp") || arg.startsWith("prepadjp")) {
        val args = arg.trim.substring(arg.trim.indexOf('(')+1, arg.trim.indexOf(')')).split(",").map(_.trim)
        if (args.size != 2) {
          None
        } else {
          val translated = translateCase(args(1))
          if (translated == "unknown") None
          else {
            val pred = arg.trim.substring(0, arg.trim.indexOf('('))
            Some(Seq(s"$pred(${args(0)},$translated)"))
          }
        }
      } else {
        None
      }
    }
  }

  object SentArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg.startsWith("sentp")) {
        val conj = arg.substring(arg.indexOf('(') + 1, arg.indexOf(')'))
        if (conj == "pz")
          Some(Seq(s"cp(int)"))
        else
          Some(Seq(s"cp($conj)"))
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

  object AdvpArgument {
    def unapply(arg: String): Option[Seq[String]] = {
      if (arg == "advp") {
        Some(Seq("xp(locat)", "xp(temp)", "xp(dur)", "xp(mod)"))
      } else {
        None
      }
    }
  }

}