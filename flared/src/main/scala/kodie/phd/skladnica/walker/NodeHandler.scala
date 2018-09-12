package kodie.phd.skladnica.walker

import scala.xml.NodeSeq
import kodie.phd.skladnica.SkladnicaXMLImplicits._

/**
 * A node handler for SkladnicaTreeWalker. It defines functions that should be fired when entering the node and when
 * choosing to go down to the child.
 *
 * In the future it is possible to add handler function for exiting.
 *
 * Created by kodie on 6/28/14.
 */
trait NodeHandler {
  /** Type alias for currently visited node */
  type CurrentNode = NodeSeq
  /** Type alias for child node */
  type ChildNode = NodeSeq

  type EnterFunction = PartialFunction[CurrentNode, Iterator[NodeHandler]]
  type ChildFunction = PartialFunction[(CurrentNode, ChildNode), Iterator[NodeHandler]]

  var enterHandler: EnterFunction = PartialFunction.empty
  var childHandler: ChildFunction = PartialFunction.empty

  /**
   * Call onEnter to add handler for node entering event. Takes partial function as parameter
   *
   * This EnterFunction takes as paramenter current node and should return the iterator of node handlers that should
   * be used during traversing the childs. You can use predefined constants (TRAVERSE, STOP_TRAVERSING, CONTINUE_WITH,
   * TRAVERSE_ALSO_WITH)
   *
   * When partial function onEnter is not defined, TRAVERSE value is returned (so traversing is continued)
   *
   * @param function
   */
  def onEnter(function: EnterFunction) {
    enterHandler = enterHandler.orElse(function)
  }

  /**
   * Call onChild to add handler for child entering event. Thanks to this function you can limit handler to specified
   * set of children (see TerminalHeadLocator).
   *
   * When partial function onChild is not defined, TRAVERSE value is returned (so traversing is continued)
   *
   * @param function
   */
  def onChild(function: ChildFunction) {
    childHandler = childHandler.orElse(function)
  }

  /* Interface used by SkladnicaTreeWalker */
  private[walker] def enter: EnterFunction = enterHandler.orElse {
    case _ => TRAVERSE
  }

  private[walker] def child: ChildFunction = childHandler.orElse {
    case _ => TRAVERSE
  }

  /**
   * Use within EnterFunction/ChildFunction. Stop traversing (returns empty iterator)
   */
  def STOP_TRAVERSING: Iterator[NodeHandler] = Iterator.empty

  /**
   * Use within EnterFunction/ChildFunction. Stop traversing (returns empty iterator)
   */
  def TRAVERSE: Iterator[NodeHandler] = Iterator.single(this)

  /**
   * Use within EnterFunction/ChildFunction. Stop traversing by current node handler and continues with newHandlers
   */
  def CONTINUE_WITH(newHandlers: NodeHandler*) = newHandlers.iterator

  /**
   * Use within EnterFunction/ChildFunction. Continue traversing with current node handler and adds also newHandlers
   */
  def TRAVERSE_ALSO_WITH(newHandlers: NodeHandler*): Iterator[NodeHandler] = Iterator.single(this) ++ newHandlers.iterator
}

/**
 * Helper object for matching in EnterFunction and ChildFunction... allows to match with specified category and returns
 * also a node itself
 *
 * Use in match od partial functions, e.g.:
 *    case Category(node, SkladnicaConstants.sentence) => STOP_TRAVERSING
 *
 */
object Category {
  def unapply(node: NodeSeq) = if (node.category != "") Some((node, node.category)) else None
}

