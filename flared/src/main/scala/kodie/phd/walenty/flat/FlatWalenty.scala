package kodie.phd.walenty.flat

import scala.io.Source

/**
 * Created by kodie on 11/19/14.
 */
class FlatWalenty {

  def parseLine(lineWithBom: String): Option[(String, FlatWalentyArguments)] = {
    val line = lineWithBom.replace('\ufeff', ' ').trim()
    if (line.isEmpty || line.startsWith("%")) {
      None
    } else {
      val elements = line.split(":", 3)
      var predicate = elements(0).trim()
      val aspect = elements(1).trim()
      val argumentsToParse = elements(2).trim()
      var sieArgument = false

      val args = new FlatWalentyArguments()

      if (predicate.endsWith("się")) {
        predicate = predicate.replace(" się", "")
        args.add("sie")
      }

      argumentsToParse.split("\\+").foreach { arg =>
        args.add(arg.trim())
      }


      // TODO: parse sie
      Some(s"$predicate:$aspect" -> args)
    }
  }

  def load() = {
    val parsedLines: Iterator[(String, FlatWalentyArguments)] = Source.fromFile("walenty.txt").getLines() flatMap (parseLine _)
    parsedLines.toSeq.groupBy(_._1).toMap.mapValues(_.map(_._2))
  }
}
