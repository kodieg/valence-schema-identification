package experimental

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import kodie.core.config.Configuration
import kodie.core.soa.{ServiceManager, ServiceContext}
import kodie.phd.assigner.SemanticHeadAssigner
import kodie.phd.skladnica.types.Sentence
import kodie.phd.walenty.old.{NewWalenty, WalentyArgument}
import org.apache.spark.{SparkContext, SparkConf}
import phd.sr.counts.BaseCounts
import phd.sr.data.{PredicateGroupKey, PredicateGroupDataset, WordsIndex}
import phd.sr.external.plwn.PLWordnet
import phd.sr.scorer.SplitSumCounts
import phd.sr.scorer.pd._
import phd.sr.tools.utils.{common, GlobalConfiguration, ComputeClassifiers}

import scala.collection.immutable
import scala.collection.mutable


/**
 * Created by kodie on 7/30/15.
 */
object SRPlayground extends App {
  //InvestigateTool.main(args)
  //println(new TreeCutBasedClassifier(null, null, null))

  val globalConfig = Configuration.parse[GlobalConfiguration](args)
  val context = new ServiceContext()
  val manager = new ServiceManager(context)
  common.configureServices(context, globalConfig)

  val dataset = manager.computeResource[PredicateGroupDataset]
  val wordnet = manager.computeResource[PLWordnet]

  val index = manager.computeResource[WordsIndex]

  val instance = VerbPseudoDisambiguationInstance(
    0,
    index.getIndexOrElse("iść: pewna: _: : imperf: subj{np(str)} + {xp(abl)} + {xp(adl)} + {xp(perl)}", -1),
    index.getIndexOrElse("iść: pewna: _: : imperf: subj{np(str)} + {np(dat)} + {prepnp(z,gen)}", -1),
    index.getIndexOrElse("szpital", -1),
    index.getIndexOrElse("szpital", -1),
    index.getIndexOrElse("{xp(adl)}", -1),
    index.getIndexOrElse("{prepnp(z,gen)}", -1)
  )

  val counts = manager.computeResource[BaseCounts]

  val synsets = wordnet.synsets(instance.semanticHeadLemma).toSeq
  println(synsets.map(wordnet.synsetAsString))


  println("groups", counts.directHits.keys.filter(_.predicate == instance.predicate1).map(i => index.getWord(i.groupType)).toList)

  val key = PredicateGroupKey(instance.predicate1, instance.groupType, synsets(0))
  println("directHits:", counts.directHits.get(key))
  println("predGroupCount: ", counts.predicateGroupCounts.get((instance.predicate1, instance.groupType)))
  println("uniqDirectHits", counts.uniqueDirectHits.get(key))
  println("uniqNouns", counts.uniqueNouns.get(key))
  println("directScores", counts.directScores.get(key))
  println("uniqScores", counts.uniqueScores.get(key))

  //println("samplex", counts.directHits.iterator.map(idx => idx._1.synset -> wordnet.synsetAsString(idx._1.synset) -> idx._2).toList)

  //println("sample", counts.directHits.forPredicateGroupType(instance.predicate2, instance.groupType).take(10).map(kv => kv._1 -> wordnet.synsetAsString(kv._1) -> kv._2))

  val splitsum = manager.computeResource[SplitSumCounts]
  println(key)
  //println("splitsum", splitsum.counts.get(key))

  val splitsumClassifier = manager.computeResource[SplitSumVerbClassifier]
  val clarkWeirClassifier = manager.computeResource[ClarkWeirVerbClassifier]
  val treeCutClassifier = manager.computeResource[TreeCutBasedClassifier]
  val selPrefClassifier = manager.computeResource[SelectionalAssociationClassifier]
  val expAClassifier = manager.computeResource[ExperimentalVerbClassifier[common.A]]
  val expBClassifier = manager.computeResource[ExperimentalVerbClassifier[common.B]]

 /* val oos = new ObjectOutputStream(new FileOutputStream("data/classifiers.obj"))
  oos.writeObject(splitsumClassifier)
  oos.writeObject(clarkWeirClassifier)
  oos.writeObject(treeCutClassifier)
  oos.writeObject(selPrefClassifier)
  oos.writeObject(expAClassifier)
  oos.writeObject(expBClassifier)
  oos.close()*/
/*

  val iis = new ObjectInputStream(new FileInputStream("data/classifiers.obj"))
  val splitsumClassifier  = iis.readObject()
  val clarkWeirClassifier = iis.readObject()
  val treeCutClassifier = iis.readObject()
  val selPrefClassifier = iis.readObject()
  val expAClassifier = iis.readObject()
  val expBClassifier = iis.readObject()

  iis.close()

  println(splitsumClassifier)
  println(expBClassifier)*/

  println("splitsum", splitsumClassifier.pickWithDebug(instance))
  println("clarkWeirClassifier", clarkWeirClassifier.pickWithDebug(instance))
  println("treeCutClassifier", treeCutClassifier.pickWithDebug(instance))
  println("selPrefClassifier", selPrefClassifier.pickWithDebug(instance))
  println("expAClassifier", expAClassifier.pickWithDebug(instance))
  println("expBClassifier", expBClassifier.pickWithDebug(instance))

  System.exit(16);


  //val res =




/*
  val (classifiers, index) = ComputeClassifiers.compute(args)
  println(classifiers, index)

*/

  /*val splitSum = classifiers(1).asInstanceOf[SplitSumVerbClassifier]
  println(instance)
  val res = splitSum.pickWithDebug(instance)*/
  //val scores = splitSum.scores
  //println("top", scores.counts.toSeq.sortBy(-_._2).take(10))

 // println(res)

  //  classifiers.
  System.exit(17)
  /*val oos = new ObjectOutputStream(new FileOutputStream("phd-classifiers-cache.obj"))
  oos.writeObject(classifiers)
  oos.writeObject(index)
  oos.close*/
  //println(index.word(1), index.word(2))
  /*final val VERBS = Set("chodzić")

  val config = new SparkConf().setMaster("local[1]").setAppName("test-app").set("spark.executor.memory", "84g")

  implicit val sc = new SparkContext(config)
  //val inputPath = args.lift(1).getOrElse("sampled.obj")
  val inputPath = "/home/kodie/sentence-with-args.obj"
  //   val inputPath = args.lift(1).getOrElse("/mnt/d1/kodie/5-preds-sentences-with-args.obj")

  val data = sc.objectFile[(Sentence, Seq[Option[Argument]])](inputPath)
  // data.sample(false, 0.01).repartition(1).saveAsObjectFile("/mnt/d1/kodie/sampled.obj")
  // System.exit(0)
  val walenty = new NewWalenty()
  val bcWalenty = sc.broadcast(walenty)
  val bcIndex = sc.broadcast(index)
  //val bcClassifiers = sc.broadcast(classifiers)


  val predicateWithArgs = data.flatMap {
    case (sentence, args) =>
      //println(sentence.text)
      //println(args)
      sentence.words.zipWithIndex.filter {
        case (word, index) => /* true */ VERBS.contains(word.base)
      } map {
        case (word, index) =>
          //println(word, index, args)
          (sentence, word, args.collect {
            case Some(arg) if arg.predicate.left == index => Some(arg.argumentType)
            case _ => None
          })
      }
  }

  val srdataset = predicateWithArgs.map {
    case (sentence, word, args) =>
      val predicateBase = word.base
      val predicateOrth = word.orth
      val aspect = if (word.ctag.contains("imperf")) "imperf" else "perf"
      // TODO: no support for sie (e.g. dać się)
      val argsLabels = args.map(_.getOrElse("_"))
      val semheads = SemanticHeadAssigner.assignToArgs(sentence.words, argsLabels)
      val frames = bcWalenty.value.findNonEmptyMatchingFrames(predicateBase, aspect, args)

      //val predBaseIdx = bcIndex.value.getIndexOrElse(predicateBase, -1)
      //val predOrthIdx = predBaseIdx // orth should not be used!

      val argTypeWithSemHead = args.zip(semheads).map {
        case (Some(argType), Some(semhead)) => Some(bcIndex.value.getIndexOrElse(argType, -1) -> bcIndex.value.getIndexOrElse(sentence.words(semhead).base, -1))
        case _ => None
      }

      println("\n=========================================\n")
      println(sentence.text)
      println(s"Predicate: $predicateBase")
      println("Arguments: " + args.zip(semheads).map {
        case (Some(argType), Some(semhead)) => Some(s"$argType -> ${sentence.words(semhead)}")
        case _ => None
      }.flatten.mkString(", "))

      // TODO: I could use bcWalenty.chooseMaximalFrame and check if it is None and only then use Chooser
      //Chooser.pick(frames, bcIndex.value, bcClassifiers.value, predicateBase, aspect, argTypeWithSemHead)
  }


  srdataset.take(100)



  /* val (classifiers, index) = readClassifiers("phd-classifiers-cache.obj")

   def readClassifiers(path: String) : (Seq[ScoreBasedVerbClassifier], WordsIndex) = {
     var ois: ObjectInputStream = null
     try {
       ois = new ObjectInputStream(new FileInputStream(path))
       val obj = ois.readObject()
       val wordsIndex = ois.readObject().asInstanceOf[WordsIndex]

       val classifiers: Seq[ScoreBasedVerbClassifier] = obj.asInstanceOf[Seq[ScoreBasedVerbClassifier]]
       (classifiers, wordsIndex)
     } finally {
       if (ois != null) ois.close
     }
   }*/
   */
}

object Chooser {
  def pick(frames: immutable.IndexedSeq[(IndexedSeq[WalentyArgument], Seq[Option[WalentyArgument]])], index: WordsIndex, classifiers: Seq[ScoreBasedVerbClassifier],
           predicateBase: String, aspect: String, argTypeWithSemHead: Seq[Option[(Int, Int)]]) = {
    val frameNames = frames.map(_._1).map { frame =>
      val frameStr = frame.mkString(" + ")
      val srLemma = s"$predicateBase:$aspect:[$frameStr]"
      srLemma
    }

    println(s"Possible frames: \n\t${frameNames.mkString("\n\t")}")

    val frameIndexes = frameNames.map(index.getIndexOrElse(_, -1)).toIndexedSeq

    println(s"Their indexes: $frameIndexes")  // -1 means that either frameName is wrong or there was no example!

    val args = argTypeWithSemHead.flatten

    val votes = mutable.Map[Int, Int]().withDefaultValue(0)

    // perform voting on all arguments
    for ((argType, semhead) <- args) {
      for (i <- 0 until frameNames.size; j <- i until frameNames.size) {
        val pred1 = frameIndexes(i)
        val pred2 = frameIndexes(j)

        val instance = VerbPseudoDisambiguationInstance(-1, pred1, pred2, semhead, semhead, argType)

        for (c <- classifiers) {
          val (which, debug) = c.pickWithDebug(instance)
          //println("Debug!", debug)
          if (which == 0) votes(i) += 1
          else if (which == 1) votes(j) += 1
          else {
            // which == -1 --> unknown sem head?
          }
        }
      }
    }

    println(s"Votes: \n\t${votes.map { case (i,votesCnt) => s"${frameNames(i)} -> $votesCnt}" }.mkString("\n\t")}")

    if (votes.nonEmpty) {
      val winnerFrame = votes.maxBy(_._2)._1
      val winnerFrameStr = frameNames(winnerFrame)

      println(s"And the winner is: $winnerFrameStr")

      winnerFrameStr
    } else {
      s"$predicateBase:$aspect:[unknownFrame]"
    }

  }

}