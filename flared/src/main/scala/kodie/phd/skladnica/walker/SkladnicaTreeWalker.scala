package kodie.phd.skladnica.walker

import scala.xml.NodeSeq
import kodie.phd.skladnica.SkladnicaXMLImplicits._

/**
 * Walks over the parse tree and fires events on handlers
 * Created by kodie on 6/28/14.
 */
object SkladnicaTreeWalker {

  def start(root: NodeSeq, initialHandlers: List[NodeHandler]) = {
    val start = (root.node("0")).head
    walk(root, start, initialHandlers)
  }

  def walk(root: NodeSeq, current: NodeSeq, currentHandlers: List[NodeHandler]) {
    require(current.size == 1)

    // Fire enter event for each handler
    val nextHandlers = currentHandlers.flatMap(_.enter(current))

    val children = current \ "children" \@("chosen", _ == "true")
    for (child <- children \ "child") {
      val childNode = root.node(child.id)

      // Fire child event for each handler
      val nextChildHandlers = nextHandlers.flatMap(_.child(current, child))
      walk(root, childNode, nextChildHandlers)
    }
  }
}

