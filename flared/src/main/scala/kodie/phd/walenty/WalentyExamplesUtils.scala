package kodie.phd.walenty

import kodie.phd.walenty.NewWalentyParser.BracketAwareSplitter

/**
 * Created by kodie on 2/25/16.
 */
object WalentyExamplesUtils {
  def argsToWalentyPositions(args: String) = {
    def parseSingleArg(arg: String) = {
      val start = arg.indexOf('[') + 1
      val end = arg.indexOf("} <") + 1
      //println(s"IN $arg --> ${arg.substring(start, end).trim()}")

      arg.substring(start, end).trim()
    }
    BracketAwareSplitter('+', args).map(parseSingleArg)
  }

  def frameContainsAllPositions(frame: WalentyFrame, positions: Seq[String]) = {
    def cleanup(s: String) = {
      s.replace(" ", "").trim()
    }

    val framePositions = frame.arguments.map(a => cleanup(a.text)).toSet
    positions.forall { pos =>
      framePositions.contains(cleanup(pos))
    }
  }
}
