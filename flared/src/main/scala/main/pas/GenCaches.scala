package main.pas

import java.io.PrintWriter

import org.apache.spark.{SparkContext, SparkConf}

/**
 * Created by kodie on 9/20/15.
 */
object GenCaches extends App {

  val config = new SparkConf().setMaster("local[*]").setAppName("test-app").set("spark.executor.memory", "40g")
  implicit val sc = new SparkContext(config)

  val input = sc.textFile("../Flared-resources/cache/data.txt").cache()
  val frameCounts = input.filter(_.trim.isEmpty).map(_.split("\t")(0)).countByValue().toSeq.sortBy(-_._2)
  val frameGroupCounts = input.filter(_.trim.isEmpty).map { x =>
    val parts = x.split("\t")
    x(0) -> x(4)
  }.countByValue().toSeq.sortBy(-_._2)


  val frameCountsOut = new PrintWriter("../Flared-resources/cache/frames.verbs")
  frameCounts.foreach { x =>
    frameCountsOut.println(s"${x._2} ${x._1}")
  }
  frameCountsOut.close()

  val frameGroupCountsOut = new PrintWriter("../Flared-resources/cache/frames.verbGroups")
  frameGroupCounts.foreach { x =>
    frameGroupCountsOut.println(s"${x._2} ${x._1._1}\t${x._1._2}")
  }
  frameGroupCountsOut.close()
}
