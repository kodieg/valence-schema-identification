package experimental

import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.skladnica.types
import kodie.phd.utils.NKJP300Freq
import kodie.phd.walenty._
import org.apache.spark.{SparkConf, SparkContext}
import phd.sr.counts.BaseCounts
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.pd._
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer


case class PredCountResult(predicate: String, classifier: Int, result: Float)

object AnalyzeResults extends App {
//  val (newArgs, path) = if (args(0) == "--input") {
//    (args.drop(2), Some(args(1)))
//  } else {
//    (args, None)
//  }



  val globalConfig = Configuration.parse[GlobalConfiguration](args)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]

  dataset.foreach { case x =>
  }

  val wordnet = manager.computeResource[PLWordnet]

//  val selAssoc = manager.computeResource[SelectionalAssociationClassifier]

  val index = manager.computeResource[WordsIndex]

  val nbClassifier = new NaiveBayesArgumentsClassifier(manager.computeResource[PredicateGroupDataset], index, manager.computeResource[BaseCounts].directScores)

  val walenty = NewWalentyParser()
  val frames = walenty.framesFor("dopłynąć", "perf", false).filter(x => x.frameText.contains("xp(adl)") && (x.frameText.contains("np(dat)") || x.frameText.contains("np(inst)")) )
  val frame = frames.find(x => x.frameText.contains("np(dat)")).get
  val arg1 = frame.arguments.find(_.text.contains("np(dat)")).get
  val arg2 = frame.arguments.find(_.text.contains("xp(adl)")).get
  val p1 = nbClassifier.prob(frame, frames, Set(arg1))
  val p2 = nbClassifier.prob(frame, frames, Set(arg2))

  println(s"frame: $frame\n arg: $arg1 -> $p1\n arg: $arg2 -> $p2")

  val path = None
  val config = new SparkConf().setMaster("local[40]").setAppName("test-app").set("spark.executor.memory", "50g")
  implicit val sc = new SparkContext(config)
  val sqlContext = new org.apache.spark.sql.SQLContext(sc)

  import sqlContext.implicits._

  val data  = sc.objectFile[(Boolean, ((types.Sentence, Seq[ArrayBuffer[ArgumentWithProb]]), Map[String, String]), ((Option[MaximalFrameChooserResult], MaximalFrameChooserDebugInfo), Int))  ](path.getOrElse("results.bin")).cache()
  import org.apache.spark.sql.functions.{col, count, mean, sum}
  // TODO: num schemas, num Walenty examples, avg num examples per schema in Walenty, result sum, TODO: maybe group by that per schema as well??!
  val df = data.map {
    case (correct, input, (output, clsId)) =>
      val predicate = input._2("haslo")

      PredCountResult(predicate, clsId, if (correct) 1.0f else 0.0f)
  }.toDF().groupBy("predicate", "classifier").agg(count(col("result")), mean(col("result")), sum(col("result"))) //.agg("result" -> "avg", "result" -> "sum", "result" -> "count")


  def xcount(predicate: String) = {
    val count = NKJP300Freq.freq.getOrElse(predicate, {
      println(s"No counts for $predicate -- either error in predicate or NKJP is too small")
      0
    })
    count
  }


  data.filter(x => (x._3._2 == 4) && x._1 && x._3._1._2.events.size >= 2).collect().foreach {
    case (_, ((sentence, args), walentyAnn), output) =>
      println(s"==============================================================\n\n${sentence.text}")
      println(s"HASLO: ${walentyAnn("haslo")}")
      println(s"RAMKA: ${walentyAnn("ramka")}")
      println(s"ARGS: ${args.flatten.map(arg => s"${arg.argumentType}:${arg.semanticHead.map(idx => sentence.words(idx).base)}").mkString(", ")}")
      println(s"CLASSIFIER: ${output._2}")

      val debugInfo = output._1._2

      val frames = debugInfo.events.map(_.frame).toSeq

      debugInfo.events.foreach { event =>

        val frame = event.frame

        val matchedRepr = event.matchedElements.collect {
          case (Some(arg), Some(walenty)) =>
            val predicateIdx = index.getIndexOrElse(event.frameString, -1)
            val headLemma = index.getIndexOrElse(arg.semanticHead.map(i => sentence.words(i).base).getOrElse("??????????!!!!"), -1)
            val groupType = index.getIndexOrElse(walenty.text, -1)
            val instance = VerbPseudoDisambiguationInstance(0, predicateIdx, predicateIdx, headLemma, headLemma, groupType, groupType)
            //val assoc = selAssoc.pickWithDebug(instance)._2._1._2
            val aprob = nbClassifier.prob(frame, frames, Set(walenty))

            val err = if (headLemma == -1 || groupType == -1 || predicateIdx == -1) "missing vals" else ""

            s"($err ${arg.argumentType} -> ${walenty.modifiers.mkString(",")}{${walenty.realizations.mkString(";")}} @ prob: $aprob" // @ assoc: $assoc)"
        }.mkString(", ")

        val args = event.matchedElements.flatMap(_._2).toSet
        val frameProb = nbClassifier.prob(frame, frames, args.toSet)
        val negArgs = frame.arguments.toSet diff args
        val negFrameProb = nbClassifier.prob(frame, frames, negArgs)

        val negDebugArgs = negArgs.toSeq.map(x => s"-${x.text} -@> ${nbClassifier.prob(frame, frames, Set(x))}}").mkString("(", ", ", ")")


        //println(s"\t${event.frameString}   ${event.info}    ${matchedRepr}     $negDebugArgs ---- pr: $frameProb, negpr: $negFrameProb, score: ${frameProb * (1-negFrameProb)}")
        println(s"\t${event.frameString} (swapper: ${event.frameSwapper})   ${event.info}    ${matchedRepr}  ${event.detailerDebug.mkString(";")}")
      }
      println("")
  }

  import org.apache.spark.sql.functions.udf

  val udfCount = udf(xcount _)

  val ext = df.withColumn("nkjp_count", udfCount($"predicate"))

  ext.repartition(1).write.format("com.databricks.spark.csv").option("header", "true").mode("overwrite").save("predicate-count-precision.csv")

  data.collect {
    case (correct, input, (output, clsId)) if input._2.getOrElse("haslo", "") == "uzyskać" =>
      val c = if (correct) "[  OKI  ]" else "[ WRONG ]"
      val expected = input._2.getOrElse("ramka", "<brak>")
      val got = output._1.map(f => s"$clsId: ${f.frame.frameText}").getOrElse("None")

      s"$c -- ${input._1._1.text}\nExpected: $expected\nGot: $got"
  }.collect().foreach(println _)

}
