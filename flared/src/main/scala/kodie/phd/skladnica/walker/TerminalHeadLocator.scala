package kodie.phd.skladnica.walker

import kodie.phd.skladnica.SkladnicaXMLImplicits._
import scala.xml.NodeSeq

/**
 * Traverses only the head path and calls update function when reaching the
 * Created by kodie on 6/28/14.
 */
abstract class TerminalHeadLocator extends NodeHandler {
  onEnter {
    case (current: CurrentNode) if current.isTerminal =>
      update(current)
      STOP_TRAVERSING
  }

  /*
  Called when head terminal is found
   */
  def update(head: NodeSeq): Unit

  onChild {
    case (current: CurrentNode, child: ChildNode) =>
      if (child.isHead)
        TRAVERSE
      else
        STOP_TRAVERSING
  }
}
