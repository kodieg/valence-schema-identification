package kodie.phd.skladnica.phrases

import kodie.phd.skladnica.walker.{Category, NodeHandler}
import kodie.phd.skladnica.SkladnicaConstants

import kodie.phd.skladnica.SkladnicaXMLImplicits._
import kodie.phd.skladnica.types.Span
import scala.collection.mutable.ArrayBuffer

/**
 * Created by kodie on 7/3/14.
 */
private[skladnica] class FreePhraseHandler(phraseSpan: Option[Span], reqType: Option[String] = None, parent: Option[(String, Span, TerminalHeadStorer)] = None) extends NodeHandler {
  def this (phraseSpan: Span) = this(Some(phraseSpan))

  var type_ = "unknown"

  val argumentsBuffer = new ArrayBuffer[(String, Span, TerminalHeadStorer)]
  val children = new ArrayBuffer[FreePhraseHandler]

  val improperHeads = Set("conj", "interp")

  def arguments : Traversable[(String, Span, TerminalHeadStorer)] = {
    argumentsBuffer.zip(children) flatMap {
      case (arg, children) => {
        // TODO: poniższa linia jest poryta
        if (arg._3.partOfSpeech.filterNot(x => improperHeads.contains(x.split(":")(0))).isDefined) {
          Seq(arg)
        } else {
          children.arguments
        }
      }
    }
  }

  val matchers = Map(
    "fno" -> caseMatcher("np") _,
    "fps" -> constMatcher("advp") _,
    "fpt" -> caseMatcher("adjp") _,
    "fpm" -> prepCaseMatcher("prepnp") _,
    "fpmpt" -> prepCaseMatcher("prepadj") _,
    "fzd" -> prepMatcher("sentp") _
  )

  onEnter {
    case Category(current, category) if matchers.contains(category) && !isCompound(current) =>
      val type_ = reqType.getOrElse { matchers(category)(current) }
      /*reqType.foreach { req =>
        if(req != type_) {
          println("WARNING! WARNING! WARNING!")
          println(s"WARNING! Expected: $req got $type_ in $current ... Ignoring error! not failing!")
        }
      }*/
      val argumentSpan = phraseSpan.getOrElse { current.spanIndex }
      val syntacticHeadFinder = new TerminalHeadStorer
      // parent.foreach { parent => arguments -= (parent) }
      val arg = (type_, argumentSpan, syntacticHeadFinder)
      argumentsBuffer += (arg)
      val handler = new FreePhraseHandler(phraseSpan, reqType, Some(arg))
      children += handler
      CONTINUE_WITH(syntacticHeadFinder, handler)
  }

  def isCompound(node: CurrentNode) = {
    // There are some adjuncts that are coordinated (w niebie i poza nim...)
    // this should allow to treat this phrase as two adjuncts
    val skladnicaCase = (node \\ "f" \@("type", _ == SkladnicaConstants.CASE)).text
    skladnicaCase.startsWith("[")
  }

  def caseMatcher(prefix: String)(node: CurrentNode) = {
    val skladnicaCase = (node \\ "f" \@("type", _ == SkladnicaConstants.CASE)).text
    val phraseCase = skladnicaCase // SkladnicaConstants.CASES(skladnicaCase)
    s"$prefix($phraseCase)"
  }

  def prepMatcher(prefix: String)(node: CurrentNode) = {
    val preposition = (node \\ "f" \@("type", _ == "tfz")).text
    s"$prefix($preposition)"
  }

  def prepCaseMatcher(prefix: String)(node: CurrentNode) = {
    // TODO: problem in adjuct phrases there exists a case "pop" (for Po prostu phrase... Po is acc, what to do?)
    // For now a new case is added (pop... but I'm not sure if it is correct)
    try {
      val skladnicaCase = (node \\ "f" \@("type", _ == SkladnicaConstants.CASE)).text
      val phraseCase = skladnicaCase // SkladnicaConstants.CASES(skladnicaCase)
      val preposition = (node \\ "f" \@("type", _ == SkladnicaConstants.PREPOSITION)).text
      s"$prefix($preposition,$phraseCase)"
    } catch {
      case e => println(prefix, node); throw e
    }
  }

  def constMatcher(value: String)(node: CurrentNode) = {
    value
  }

  def buildType = type_
}

