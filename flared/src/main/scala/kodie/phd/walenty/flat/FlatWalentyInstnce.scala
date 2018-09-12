package kodie.phd.walenty.flat

import kodie.phd.skladnica.SkladnicaConstants

/**
 * Created by kodie on 11/19/14.
 */
object FlatWalentyInstnce extends FlatWalenty {
  val data = load()

  def checkArgument(predicateLemma: String, predicateAspect: String, skladnicaArgumentType: String) : Boolean = {
    val possibleFrames = data.getOrElse(s"$predicateLemma:$predicateAspect", Seq()) ++ data.getOrElse(s"$predicateLemma:_", Seq())
    val flattenedArguments = possibleFrames.flatMap(_.arguments)
    // println(flattenedArguments.toList)
    WalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty(skladnicaArgumentType, flattenedArguments)
  }

  def hasPredicate(predicateLemma: String, predicateAspect: String) = {
    data.contains(s"$predicateLemma:$predicateAspect") || data.contains(s"$predicateLemma:_")
  }
}

object WalentySkladnicaBridge {
  def tryToMatchSkladnicaAndWalenty(skladnicaArgument: String, walentyArguments: Seq[String]) = {
    // We do not suppoer xp arguments to well
    skladnicaArgument match {
      case "subj" => walentyArguments.contains("subj")
      case "sie" => walentyArguments.contains("sie") || walentyArguments.contains("refl")
      case NominalArgument(p) => p.exists(walentyArguments.contains _)
      case AdjectivalArgument(p) => p.exists(walentyArguments.contains _)
      case PrepositionalArgument(p) => p.exists(walentyArguments.contains _)
      case SentArgument(p) => p.exists(walentyArguments.contains _)
      case InfArgument(p) => p.exists(walentyArguments.contains _)
        // problem with xp(*) - that are realized by prepositional
      // case AdvpArgument(p) => p.exists(walentyArguments.contains _)
        // argument {or} -- mowa zależna
        // preplexnp?
        // prepncp -- czy w formalizmie składnicy w ogóle jest opowienik?
      case _ => false
    }
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
