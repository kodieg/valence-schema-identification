package kodie.phd.walenty

import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


case class WalentyArgument(text: String, modifiers: Seq[String], realizations: Seq[String])
case class WalentyFrame(frameText: String, predicate: String, status: String, neg: String, aspect: String, arguments: Array[WalentyArgument])

class NewWalentyParser(@transient source: Source) extends Serializable {

  val frames = {
    val frameLines = source.getLines().filterNot(_.trim.startsWith("%")).filterNot(_.replace('\ufeff', ' ').trim.isEmpty)
    frameLines.map(parseFrame).toSeq.groupBy(_.predicate).toMap
  }


  def framesFor(predicate: String, aspect: String, hasSieArgument: Boolean) = {
    // TODO: this make sie argument special due to the fact that it has to be choosen
    val predicateFrames = frames.getOrElse(predicate, Seq()).filter(fr => Set(aspect, "_").contains(fr.aspect))

    val allFrames = if (hasSieArgument) {
      val predicateFramesWithSie = predicateFrames.filter(_.arguments.exists(isSieArgument))
      val predicateWithSieFrames = frames.getOrElse(s"$predicate się", Seq()).filter(fr => Set(aspect, "_").contains(fr.aspect))

      predicateWithSieFrames ++ predicateFramesWithSie
    } else {
      predicateFrames
    }
    // THIS IS HACK AS I DON'T RECOGNIZE {or}
    allFrames.filterNot(_.frameText.contains("{or}"))
  }

  private[this] def isSieArgument(arg: WalentyArgument) = {
    arg.realizations.contains("refl")
  }


  private[this] def parseFrame(frameLineWithBOM: String) = {
    val line = frameLineWithBOM.replace('\ufeff', ' ').trim()
    val List(predicate, status, neg, _, aspect, argumentsString) = line.split(":", 6).toList
    val arguments = BracketAwareSplitter('+',argumentsString).map(_.trim).map(parseArgument)
    WalentyFrame(line, predicate.trim(), status.trim(), neg.trim(), aspect.trim(), arguments)
  }

  private[this] def parseArgument(argumentSpec: String) = {
    val determinersEnd = argumentSpec.indexOf('{')
    val determiners = argumentSpec.substring(0, determinersEnd).split(",").map(_.trim()).filter(!_.isEmpty).toArray
    val argumentRealizationSpecs = argumentSpec.substring(determinersEnd+1, argumentSpec.length-1)
    val argumentRealizations: IndexedSeq[String] = BracketAwareSplitter.apply(';', argumentRealizationSpecs).map { arg =>
      arg.trim()
    }.toArray[String]

    WalentyArgument(argumentSpec.trim, determiners, argumentRealizations)
  }
}

object NewWalentyParser {
  object BracketAwareSplitter {
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
       /* else if (c == '\'' && inStr) skipping -= 1
        else if (c == '\'' && !inStr) skipping += 1*/
        else if (skipping == 0 && sep == c) {
          buffer += string.substring(lastSep, i)
          lastSep = i + 1
        }
      }
      if (lastSep != size) buffer += string.substring(lastSep, size)
      buffer.toArray
    }
  }
  def apply() = {
    new NewWalentyParser(Source.fromFile("data/walenty_20160205_verbs_all.txt"))
//    new NewWalentyParser(Source.fromFile("verbs/walenty_20150331_verbs_all.txt"))

  }
}

/*
object Test extends App {
  val frames = NewWalentyParser().frames
  println(s"Loaded frames: ${frames.size}")
  frames.foreach { f =>
    println(s"\nEntry ${f.frameText}")
    println(s"Predicate: ${f.predicate}:${f.aspect}   Status: ${f.status}   Neg: ${f.neg}")
    f.arguments.foreach { arg =>
      println(s"\t${arg.modifiers} relizations: ${arg.realizations}")
    }
  }
}
*/