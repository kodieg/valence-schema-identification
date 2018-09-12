package kodie.phd.skladnica.walker

import org.scalatest.{Matchers, FlatSpec}
import kodie.phd.skladnica.SkladnicaConstants
import kodie.phd.skladnica.SkladnicaXMLImplicits._

/**
 * Created by kodie on 7/3/14.
 */
class TestNodeHandler extends FlatSpec with Matchers {
  "CategoryExtractor" should "match category nodes correctly" in {
    val input = <node><nonterminal><category>zdanie</category></nonterminal></node>

    input match {
      case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) => assert(current == input)
      case _ => fail("Not matched")
    }
  }

  "CategoryExtractor" should "not match if category differs" in {
    val input = <node><nonterminal><category>other</category></nonterminal></node>

    input match {
      case Category(current, SkladnicaConstants.SENTENCE_CATEGORY) => fail("Matched incorrectly")
      case _ => assert(true)
    }
  }

  "NodeHandler.enter" should "call onEnter partial" in {
    var called = false

    class TestHandler extends NodeHandler {
      onEnter {
        case _ => called = true; TRAVERSE
      }
    }

    val handler = new TestHandler
    val result = handler.enter(<node></node>)

    assert(called)
    assert(result.toList == List(handler))
  }

  "NodeHandler.enter" should "return self handler if partial nod matched" in {
    var called = false

    class TestHandler extends NodeHandler {
      onEnter {
        case x if x.id == "0" => called = true; STOP_TRAVERSING
      }
    }

    val handler = new TestHandler
    val result = handler.enter(<node nid="1"></node>)

    assert(!called)
    assert(result.toList == List(handler))
  }
}
