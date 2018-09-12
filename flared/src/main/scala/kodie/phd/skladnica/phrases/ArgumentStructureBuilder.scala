package kodie.phd.skladnica.phrases

import kodie.phd.skladnica.walker.{TerminalHeadLocator, Category, NodeHandler}
import scala.collection.mutable
import kodie.phd.skladnica.SkladnicaConstants
import scala.xml.NodeSeq
import scala.collection.mutable.ArrayBuffer
import kodie.phd.skladnica.SkladnicaXMLImplicits._
import kodie.phd.skladnica.types._

object CONFIG {
  val EXTRACT_ARGUMENTS_OF_INFP = true
}

class TerminalHeadStorer extends TerminalHeadLocator {
  var syntacticHead : Option[Index] = None
  var partOfSpeech: Option[String] = None

  override def update(head: NodeSeq) = {
    syntacticHead = Some(head.fromIndex)
    partOfSpeech = Some(head \\ "f" \@ ("type", _ == "tag") text)
  }
}

class PredicateFinder extends NodeHandler {
  var predicateSpan: Option[Span] = None

  onEnter {
    case Category(current, SkladnicaConstants.FINITIVE_PHRASE_CATEGORY) =>
      predicateSpan = Some(current.spanIndex)
      STOP_TRAVERSING
    case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) => STOP_TRAVERSING
  }
  onChild {
    case (current: CurrentNode, child: ChildNode) =>
      if (child.isHead)
        TRAVERSE
      else
        STOP_TRAVERSING
  }
}

class FreePhraseLocator extends NodeHandler {
  val handlers = ArrayBuffer[FreePhraseHandler]()
  onEnter {
    case Category(current, SkladnicaConstants.FREE_PHRASE_CATEGORY) =>
      val handler = new FreePhraseHandler(current.spanIndex)
      handlers += handler
      CONTINUE_WITH(handler)
    case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) => STOP_TRAVERSING
  }
}

private[skladnica] class FreePhraseStructureHandler extends NodeHandler {
  val structureHandlers = mutable.ArrayBuffer[(FreePhraseLocator, PredicateFinder)]()

  onEnter {
    case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) =>
      val structureHandler = new FreePhraseLocator()
      val predicateFinder = new PredicateFinder()
      structureHandlers += ((structureHandler, predicateFinder))
      TRAVERSE_ALSO_WITH(structureHandler, predicateFinder)
    case Category(current, cat) if (CONFIG.EXTRACT_ARGUMENTS_OF_INFP && (cat == SkladnicaConstants.REQUIRED_PHRASE_CATEGORY && isInfinitivePhrase(current))) =>
      val structureHandler = new FreePhraseLocator()
      val predicateFinder = new PredicateFinder()
      // hack! to keep existing interface
      val terminalHead = new TerminalHeadLocator {
        override def update(head: NodeSeq): Unit = predicateFinder.predicateSpan = Some(head spanIndex)
      }
      structureHandlers += ((structureHandler, predicateFinder))
      TRAVERSE_ALSO_WITH(structureHandler, terminalHead)
  }

  def isInfinitivePhrase(current: NodeSeq) =  (current \\ "f" \@ ("type", _ == "tfw") text) startsWith("infp")

  def buildStructures() = {
    structureHandlers.flatMap {
      case (freePhraseHandler, predicateFinder) =>
        freePhraseHandler.handlers.flatMap(_.arguments.map {
          case (type_, argSpan, headLocator) =>
            //println(predicateFinder.predicateSpan -> Argument(argSpan, type_, headLocator.syntacticHead, None))
            predicateFinder.predicateSpan -> Argument(argSpan, type_, headLocator.syntacticHead, None)
        })
    }.groupBy(_._1).map(kv => kv._1.get -> kv._2.map(_._2).toSeq)
  }
}

private[skladnica] class SentencePhraseHandler extends NodeHandler {
  val structureHandlers = mutable.ArrayBuffer[StructurePhrasesHandler]()

  onEnter {
    case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) =>
      val structureHandler = new StructurePhrasesHandler(StructureBuilder())
      structureHandlers += structureHandler
      TRAVERSE_ALSO_WITH(structureHandler)
    case Category(current, cat) if (CONFIG.EXTRACT_ARGUMENTS_OF_INFP && cat == SkladnicaConstants.REQUIRED_PHRASE_CATEGORY && isInfinitivePhrase(current)) =>
      val sb = StructureBuilder()
      val headLocator = new TerminalHeadLocator {

        override def update(head: NodeSeq): Unit = sb.predicate = Some(head spanIndex)
      }
      val structureHandler = new StructurePhrasesHandler(sb)
      structureHandlers += structureHandler
      TRAVERSE_ALSO_WITH(structureHandler, headLocator)
  }

  def isInfinitivePhrase(current: NodeSeq) =  (current \\ "f" \@ ("type", _ == "tfw") text) startsWith("infp")


  def buildStructures() = {
    val structurePairs = structureHandlers.flatMap(_.buildStructure)
    val byPredicate = structurePairs.groupBy(_._1)
    val mergedArguments = byPredicate.map(x => (x._1, x._2.map(_._2).flatten.toSeq)).toMap
    mergedArguments
  }
}


private[skladnica] class StructurePhrasesHandler(val builder: StructureBuilder) extends NodeHandler {
  val SEPARATE_ARGS = true
  onEnter {
    case Category(current, SkladnicaConstants.REQUIRED_PHRASE_CATEGORY) =>
      val argument = ArgumentBuilder(current)
      if (argument.argumentType == "advp") {
        builder.arguments += argument
        argument.advpHandler = Some(new FreePhraseHandler(current spanIndex))
        CONTINUE_WITH(new ArgumentBuilderHandler(argument), argument.advpHandler.get)
      } else if (SEPARATE_ARGS) {
        argument.extraHandler = Some(new FreePhraseHandler(None, Some(argument.argumentType), None))
        builder.arguments += argument
        CONTINUE_WITH(argument.extraHandler.get, new ArgumentBuilderHandler(argument))
      } else {
        builder.arguments += argument
        CONTINUE_WITH(new ArgumentBuilderHandler(argument))
      }
    case Category(current, SkladnicaConstants.FINITIVE_PHRASE_CATEGORY) =>
      builder.predicate = Some(current.spanIndex)
      TRAVERSE
    case Category(_, category) if Set("fl", "zdanie", "formarzecz", "formaprzym") contains category =>
      STOP_TRAVERSING
  }

  def buildStructure = builder.build
}

private[skladnica] class SentPArgumentBuilderHandler(val builder: StructureBuilder, parent: Option[ArgumentBuilder] = None) extends NodeHandler {
  onEnter {
    case Category(current, SkladnicaConstants.SUBSENTENCE_CATEGORY) =>
      val tfz = current \\ "f" \@ ("type", _ == "tfz") text
      val type_ = s"sentp($tfz)"
      val argument = ArgumentBuilder(current, None, None, Some(type_))
      // Parent fzd contains both this argument and probably some more, we want finest granularity
      parent.foreach{ parent => builder.arguments -= parent }
      builder.arguments += argument

      CONTINUE_WITH(new ArgumentBuilderHandler(argument), new SentPArgumentBuilderHandler(builder, Some(argument)))
  }
}


private[skladnica] class ArgumentBuilderHandler(argument: ArgumentBuilder) extends TerminalHeadLocator {
  def update(head: NodeSeq) {
    argument.syntacticHead = Some(head.fromIndex)
    argument.syntacticHeadPos = Some(head \\ "f" \@ ("type", _ == "tag") text)
  }
}


private[skladnica] case class StructureBuilder(var predicate: Option[Span] = None,
                                               arguments: ArrayBuffer[ArgumentBuilder] = mutable.ArrayBuffer()) {
  def build: Option[(Span, Seq[Argument])] = predicate.map(predicate => predicate -> arguments.flatMap(_.build))
}


private[skladnica] case class ArgumentBuilder(node: NodeSeq,
                                              var syntacticHead: Option[Index] = None,
                                              var semanticHead: Option[Nothing] = None,
                                              argType: Option[String] = None,
                                               var syntacticHeadPos: Option[String] = None) {
  val argumentType = argType.getOrElse { node \\ "f" \@("type", _ == SkladnicaConstants.REQUIRED_PHRASE_TYPE_ATTRIBUTE) text }
  var advpHandler : Option[FreePhraseHandler] = None
  var extraHandler : Option[FreePhraseHandler] = None

  val improperPos = Seq("conj", "interp")
  def build = {
    extraHandler.map(_.arguments).filter(_.size > 0 && improperPos.contains(syntacticHeadPos.getOrElse("xxx"))).map(_.map {
      case (type_, span, head) => new Argument(span, type_, head.syntacticHead, semanticHead)
    }).getOrElse {
      val currentType = advpHandler.flatMap(_.arguments.headOption.map(_._1)).filter(_ != "unknown").getOrElse(argumentType)
      // println(currentType)
      val cleanedArgumentType = currentType.replace(" ", "").replace("\t", "").replace("\n", "")
      Seq(new Argument(node.spanIndex, cleanedArgumentType, syntacticHead, semanticHead))
    }
  }
}
