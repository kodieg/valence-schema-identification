package kodie.phd.walenty.flat

import scala.collection.mutable.ArrayBuffer

/**
 * Created by kodie on 11/18/14.
 */
// This is flat argument impl... sometimes I'll need original full spec i.e. full {...} definition
class FlatWalentyArguments {
  // we don't care that some arguments may exclude other
  val arguments = ArrayBuffer[String]()
  
  def add(arg: String) {
    arg match {
      case "sie" => arguments += "sie"
      case Subj(_) => arguments += "subj"
      case Obj(elems) => arguments ++= elems  // maybe prefix with obj:np(str) -- so we can later np(acc) --> obj:np(str) ?
      case Other(elems) => arguments ++= elems
    }
    //arguments += arg
  }

  def extractElems(strArg: String) = {
    val str = strArg.trim()
    assert(str(0) == '{' && str.last == '}', str)
    val cut = str.substring(1, str.length - 1)
    Some(cut.split(";").map(_.trim()).toSeq)
  }

  object Subj {
    def unapply(str: String) = {
      if (str.startsWith("subj,controller{")) extractElems(str.substring("subj,controller".size))
      else if (str.startsWith("subj,controllee{")) extractElems(str.substring("subj,controllee".size))
      else if (str.startsWith("subj{")) extractElems(str.substring(4))
      else None
    }
  }

  object Obj {
    def unapply(str: String) = {
      if (str.startsWith("obj,controller{")) extractElems(str.substring("obj,controller".size))
      else if (str.startsWith("obj,controllee,controller2{")) extractElems(str.substring("obj,controllee,controller2".size))
      else if (str.startsWith("obj,controllee{")) extractElems(str.substring("obj,controllee".size))
      else if (str.startsWith("obj{")) extractElems(str.substring(3))
      else None
    }
  }

  object Other {
    def unapply(str: String) = {
      if (str.startsWith("controller{")) extractElems(str.substring("controller".size))
      else if (str.startsWith("controllee,controller2{")) extractElems(str.substring("controllee,controller2".size))
      else if (str.startsWith("controllee{")) extractElems(str.substring("controllee".size))
      else if (str.startsWith("controllee2{")) extractElems(str.substring("controllee2".size))
      else if (str.startsWith("controller2{")) extractElems(str.substring("controller2".size))
      else if (str.startsWith("{")) extractElems(str)
      else None
    }
  }

}
