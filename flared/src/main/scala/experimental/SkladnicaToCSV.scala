package experimental

import kodie.phd.skladnica.features._
import kodie.phd.skladnica.types.Sentence
import org.apache.spark.{SparkContext, SparkConf}

// TODO: refactor; cleanup; move out of experimental (is this used anywhere?)
case class RowCSV(sentId: Int, no: Int, orth: String, base: String, ctag: String, argumentType: String, predicate: Int)

object SkladnicaToCSV extends App {
  val config = new SparkConf().setMaster("local[40]").setAppName("test-app").set("spark.executor.memory", "50g")
  implicit val sc = new SparkContext(config)
  val sqlContext = new org.apache.spark.sql.SQLContext(sc)
  import sqlContext.implicits._

  val chunkedSentencesPath = "data/Skladnica-full-pantera.dat"

  val data = sc.objectFile[Sentence](chunkedSentencesPath)

  val rows = data.zipWithIndex().flatMap { case (sentence, sentId) =>
    val labels = extractBothArgumentTypesAndAdjuncts(sentence)
    val predArgPairs = (sentence.argumentStructures.flatMap(kv => kv._2.filter(_.syntacticHead.isDefined).map(a => a.syntacticHead.get.i -> kv._1.left))
                        ++ sentence.freePhrases.flatMap(kv => kv._2.filter(_.syntacticHead.isDefined).map(a => a.syntacticHead.get.i ->  kv._1.left)))
    val predMap = predArgPairs.toMap
    sentence.words.zipWithIndex.map { case (word, no) =>
       RowCSV(sentId.toInt, no, word.orth, word.base, word.ctag, labels.lift(no).getOrElse("_"), predMap.getOrElse(no, -1))
    }
  }

  rows.toDF().repartition(1).write.format("com.databricks.spark.csv").option("header", "true").mode("overwrite").save("data/skladnica.csv")

}
