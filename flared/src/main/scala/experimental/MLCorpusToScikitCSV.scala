package experimental

import java.io.{File, FileOutputStream, OutputStream, PrintWriter}
import java.util.zip.GZIPOutputStream

import breeze.linalg.DenseVector
import kodie.core.config.Configuration
import kodie.core.soa.{ServiceContext, ServiceManager}
import kodie.phd.skladnica.types
import kodie.phd.skladnica.types.{Sentence, Word}
import kodie.phd.tools.{wordnet => wn}
import kodie.phd.walenty._
import main.pas.ExtractCorpusForML.newArgs1
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import org.apache.spark.{HashPartitioner, SparkConf, SparkContext}
import phd.sr.data.{PredicateGroupDataset, WordsIndex}
import phd.sr.scorer.pd.{ClarkWeirVerbClassifier, SplitSumVerbClassifier, VerbPseudoDisambiguationInstance}
import phd.sr.tools.utils.{GlobalConfiguration, common}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool

/**
 * Refactor... move to appropriate places...
 */
object MLCorpusToScikitCSV extends App {
  val (newArgs0, path) = if (args(0) == "--input") {
    (args.drop(2), Some(args(1)))
  } else {
    (args, None)
  }

  val (newArgs, outputPathOpt) = if (newArgs0(0) == "--output") {
    (newArgs0.drop(2), Some(newArgs0(1)))
  } else {
    (newArgs0, None)
  }

  val config = new SparkConf().setMaster("local[64]").setAppName("test-app").set("spark.network.timeout", "1000000000").set("spark.driver.maxResultSize", "50g").set("spark.rpc.askTimeout", "1200s")
  implicit val sc = new SparkContext(config)
  val inputPath = path.getOrElse("data/pantera-nkjp-300m-ml-extended-dataset.obj")
  val outputPath = outputPathOpt.getOrElse("data/ml/csv-full/")
  val corpus_ = sc.objectFile[(String, (types.Sentence, types.Word, Seq[ArrayBuffer[ArgumentWithProb]], Seq[ArrayBuffer[ArgumentWithProb]], WalentyFrame))](inputPath)
  val globalConfig = Configuration.parse[GlobalConfiguration](newArgs)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  val FRAME_SWAPPER_ON = true
  common.configureServices(context, globalConfig)

  MLCorpusToScikitCSVHelper.compute(sc, corpus_, globalConfig, manager, outputPath)
}

  object MLCorpusToScikitCSVHelper {
    final val pathTemplate = "PRED/FRAME.csv.gz"
    final val N = 10

    def compute(sc: SparkContext, corpus: RDD[(String, (types.Sentence, types.Word, Seq[ArrayBuffer[ArgumentWithProb]], Seq[ArrayBuffer[ArgumentWithProb]], WalentyFrame))],
                globalConfig: GlobalConfiguration, manager: ServiceManager, outputPrefix: String = "data/ml/csv-full"
                 ) = {

      /*corpus_.filter(_._2.base == "mówić").keyBy(_._5.frameText).countByKey().foreach { case (k,v) =>
      println(s"$k --> $v")
  }
  System.exit(0)
*/

      {
        val dataset = manager.computeResource[PredicateGroupDataset]

        dataset.foreach { case x =>
        }

      }






      val walenty = NewWalentyParser()


      //val corpus = corpus_ // .sample(false, 0.01)

      val featureExtractor = {
        val plWordnet = wn.PLWordnet.load()

        val lexToDomains = plWordnet.units.values.map(lu => lu.name -> lu.domain).groupBy(_._1).mapValues(_.map(_._2).toArray)

        Array.tabulate(N) { i =>
          println(s"Computing $i chooser")
          val chooser = new FrameChooserCascadeFactory(manager, globalConfig.countsCacheFile.getParent)

          new SchemaFeatureExtractor(lexToDomains,
            chooser.word2vec,
            chooser.index,
            new CosineSimilarity(chooser.schemaVecs),
            new LogRegressionSimilarity(chooser.logisticRegClassifiers),
            manager.computeResource[SplitSumVerbClassifier],
            manager.computeResource[ClarkWeirVerbClassifier]
          )
        }
      }

      computeSpark(sc, corpus, walenty, featureExtractor, outputPrefix)
    }

    def computeSpark(sc: SparkContext, corpus: RDD[(String, (Sentence, Word, Seq[ArrayBuffer[ArgumentWithProb]], Seq[ArrayBuffer[ArgumentWithProb]], WalentyFrame))], walenty: NewWalentyParser, featureExtractors: Array[SchemaFeatureExtractor], outputPrefix: String): Unit = {
      val mappingS = corpus//corpus.keyBy(x => x._2.base + x._5.frameText).repartitionAndSortWithinPartitions(new HashPartitioner(10000)).cache()//.sortBy(_._1).partitionBy(new HashPartitioner(10000))//collect().par
      //mappingS.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(20))
      val z = (0 until mappingS.partitions.size).toArray.par
      z.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(20))
      type T = (String, (Sentence, Word, Seq[ArrayBuffer[ArgumentWithProb]], Seq[ArrayBuffer[ArgumentWithProb]], WalentyFrame))
      val mmapping = z.map { i =>
        val fpMap = mutable.Map[String, OutputStream]()
        val fpPaths = mutable.Map[String, String]()
        var lastBase = ""
        val featureExtractor = featureExtractors(i % featureExtractors.length)
        sc.runJob(mappingS, (iter: Iterator[T]) => iter.toArray, Seq(i)).head.foreach {
          case (_, (sentence, predicate, args, extendedArgs, goldFrame)) =>
            val base = predicate.base
            if (base != lastBase) {
              fpMap.foreach {
                _._2.close()
              }
              fpMap.clear()
            }

            lastBase = base
            val aspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"
            val hasSieArgument = args.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")
            // let's ignore lex frames (we're ignoring lex frames in extract ml corps file already)
            val frames = walenty.framesFor(base, aspect, hasSieArgument)//.filter(x => !x.arguments.exists(_.realizations.exists(_.startsWith("lex"))))
            for (f <- frames) {
              val result = if (f.frameText == goldFrame.frameText) 1 else 0
              val features = featureExtractor.extract(predicate, sentence.words, args, f, result)

              val path = outputPrefix + "/" + pathTemplate.replace("FRAME", base + "-" + f.frameText.hashCode.toString + "-" + i.toString).replace("PRED", base)

              fpPaths.getOrElseUpdate(f.frameText, {
                new File(path).getParentFile.mkdirs()
                path
              })
              val fp = fpMap.getOrElseUpdate(f.frameText, {
                val out = new GZIPOutputStream(new FileOutputStream(path))
                out.write(featureExtractor.headers(f).getBytes)
                out.write("\n".getBytes)
                out
              })
              fp.write(features.getBytes())
              fp.write("\n".getBytes())
            }
        }

        fpMap.foreach(_._2.close())

        fpPaths.toSeq
      }

      val mapping = mmapping.seq.flatten

      val out = new PrintWriter(new FileOutputStream( outputPrefix + "/mapping.txt"))

      mapping.foreach { case (frame, path) =>
        out.println(s"$path  --> $frame")
      }

      out.close()
    }
  }

class SchemaFeatureExtractor( lexToDomains: Map[String, Array[String]],
                        word2vec: Map[String, DenseVector[Double]],
                        index: WordsIndex,
                        cs: VectorBasedSR,
                        logreg: VectorBasedSR,
                        splitSumVerbClassifier: SplitSumVerbClassifier,
                        clarkWeirVerbClassifier: ClarkWeirVerbClassifier
                        ) extends Serializable {
  def headers(frame: WalentyFrame): String = {
    val h = Seq("frame", "predicate_base", "predicate_orth", "predicate_aspect", "has_sie", "is_neg") ++ frame.arguments.flatMap { arg =>
      Seq("has_", "domains_", "cs_", "logreg_sim_", "clarkweir_", "splitsum_").map(_ + arg.text) /*TODO extend with more arg features*/
    } ++ Seq("target")
    h.map(_.replaceAll("\t", " ")).mkString("\t")
  }

  def extract(predicate: types.Word, words: Seq[Word], args: Seq[ArrayBuffer[ArgumentWithProb]], frame: WalentyFrame, result: Int): String = {
    val predicateBase = predicate.base
    val predicateOrth = predicate.orth.toLowerCase
    val predicateAspect = if (predicate.ctag.contains("imperf")) "imperf" else "perf"
    val frameText = frame.frameText
    val hasSieArgument = if (args.exists(_.lift(0).map(_.argumentType).getOrElse("_") == "sie")) "1" else "0"

    // this might not be 100% accurate as we dont have predicate index :(
    val isNegBool = words.zipWithIndex.filter(_._1 == predicate).exists {
      case (_, idx) =>
        words.lift(idx - 1).exists(_.base == "nie")
    }
    val isNeg = if (isNegBool) "1" else "0"

    val frameIdx0 = index.getIndexOrElse(frame.frameText, -1000)
    val frameIdx = if (frameIdx0 == -1000) None else Some(frameIdx0)


    val argumentsMatching = WalentyArgumentMatcher.matchingElements(words, args, frame)

    val argFeatures = frame.arguments.flatMap { walentyArgument =>
      val argInstances = argumentsMatching.filter(x => x.isDefined && x.get._2 == walentyArgument)
      val bases = argInstances.zip(words).collect {
        case (Some((arg, _)), word) => word.base
      }
      val domains = argInstances.zip(words).collect {
        case (Some((arg, _)), word) => lexToDomains.getOrElse(word.base, Array[String]())
      }.flatten.toSet.toSeq.sorted.map(_.replaceAll("\t", "").replaceAll(" ", "_").toLowerCase()).mkString(",")

      val groupTypeIndex0 = index.getIndexOrElse(walentyArgument.text, -1000)
      val groupTypeIndex = if (groupTypeIndex0 == -1000) None else Some(groupTypeIndex0)


      val vec = bases.flatMap(base => {
        groupTypeIndex.flatMap(gt => frameIdx.flatMap(fi => word2vec.get(base).flatMap { v => cs.maybeComputeSimilarity(fi, gt, v)}))
      })
      val csSim = if (vec.isEmpty) "NaN" else f"${vec.max}%.6f"

      val logSimTmp = bases.flatMap(base => {
        groupTypeIndex.flatMap(gt => frameIdx.flatMap(fi => word2vec.get(base).flatMap { v => logreg.maybeComputeSimilarity(fi, gt, v)}))
      })
      val logSim = if (logSimTmp.isEmpty) "NaN" else f"${logSimTmp.max}%.6f"

      val cwSimTmp = argInstances.flatten.flatMap(x => words.lift(x._1.semanticHead.getOrElse(-1))).flatMap { baseWord =>

        val baseIdx0 = index.getIndexOrElse(baseWord.base, -1000)
        val baseIdx = if (baseIdx0 == -1000) None else Some(baseIdx0)

        baseIdx.flatMap { bi =>
          groupTypeIndex.flatMap(gt => frameIdx.map { fi =>
            val instance = VerbPseudoDisambiguationInstance(-1, fi, fi, bi, bi, gt, gt)
            clarkWeirVerbClassifier.synchronized{clarkWeirVerbClassifier.pickWithDebug(instance)._2._1._2}

          })

        }
      }
      val cwSim = if (cwSimTmp.isEmpty) "NaN" else f"${cwSimTmp.max}%.6f"

      val ssSimTmp = argInstances.flatten.flatMap(x => words.lift(x._1.semanticHead.getOrElse(-1))).flatMap { baseWord =>

        val baseIdx0 = index.getIndexOrElse(baseWord.base, -1000)
        val baseIdx = if (baseIdx0 == -1000) None else Some(baseIdx0)

        baseIdx.flatMap { bi =>
          groupTypeIndex.flatMap(gt => frameIdx.map { fi =>
            val instance = VerbPseudoDisambiguationInstance(-1, fi, fi, bi, bi, gt, gt)
            splitSumVerbClassifier.synchronized{splitSumVerbClassifier.pickWithDebug(instance)._2._1._2}
          })

        }
      }
      val ssSim = if (ssSimTmp.isEmpty) "NaN" else f"${ssSimTmp.max}%.6f"

      val has = if (argInstances.size > 0) "1" else "0"


      Seq(has, domains, csSim, logSim, cwSim, ssSim /* TODO more argument features*/)

    }


    val res = if (result == 0) "0" else "1"

    // TODO more frame feeatures: eg. choose results ... or such stuff
    val allFeats = Seq(frameText, predicateBase, predicateOrth, predicateAspect, hasSieArgument, isNeg) ++ argFeatures

    (allFeats ++ Seq(res)).map(_.replaceAll("\t", " ")).mkString("\t")
  }

}
