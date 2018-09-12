package kodie.phd.walenty

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by kodie on 8/11/15.
 */
class NewWalentyRealizationList(@transient source: Source) {
//  def this() = this(Source.fromFile("realizations_20150331.txt"))
  def this() = this(Source.fromFile("phrase_types_expand_20160205.txt"))

    val realizations = {
      val mapping = mutable.Map[String, Seq[String]]()
      var walentyType : Option[String] = None
      val realizations = ArrayBuffer[String]()
      source.getLines().foreach { line =>
        if (line.startsWith(" ")) {
          realizations += line.trim().split("\\[")(0).trim() // ignore relization status for now
        } else if (line.contains("-->")) {
          walentyType.foreach { walType =>
            mapping += walType -> realizations.toIndexedSeq
          }

          walentyType = Some(line.substring(0, line.indexOf("-->")))
          realizations.clear()
        }
      }
      walentyType.foreach { walType =>
        mapping += walType -> realizations.toIndexedSeq
      }

      mapping
    }

    val   skladnicaPossibilities = {
      val one = realizations.toSeq.flatMap {
        case (walenty, skladnicaRealizations) =>
          skladnicaRealizations.map { realization =>

            (realization, walenty)

          }
      } groupBy( _._1 ) mapValues( _.map(_._2) )

      // Nested structures.... assuming depth eq 1
      val two = one.mapValues { poss =>
        poss.flatMap { entry =>
          Seq(one.get(entry).toSeq.flatten, Seq(entry)).flatten
        }
      }

      two
    }
}

object Test extends App {
  println(new NewWalentyRealizationList().skladnicaPossibilities)
}
