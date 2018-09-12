package kodie.phd.utils

import scala.io.Source

/**
 * Created by kodie on 9/8/16.
 */
object NKJP300Freq {
  val freq = Source.fromFile("data/NKJP300M_verb.txt").getLines().collect {
    case line if line.trim.nonEmpty =>
      val arr = line.split(",")
      arr(0).trim -> arr(1).trim.toInt
  }.toMap
}
