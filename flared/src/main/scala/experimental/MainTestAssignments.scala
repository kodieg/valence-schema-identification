package experimental


import kodie.phd.skladnica.SkladnicaSentenceParser
import org.apache.spark.SparkContext
import kodie.phd.skladnica.features._
import kodie.phd.utils.spark.Checkpointed
import kodie.phd.assigner._
import java.io.PrintWriter

import experimental.MLCorpusToScikitCSV.newArgs0

/**
 * Created by kodie on 1/20/16.
  * TODO: refactor (add option to switch between validation set and test set without modifying code (3/10)...
 */
object MainTestAssignments {
  def main(args: Array[String]) {
    val (newArgs0, path) = if (args(0) == "--input") {
      (args.drop(2), Some(args(1)))
    } else {
      (args, None)
    }

    val (newArgs1, outputPathOpt) = if (newArgs0(0) == "--report-output") {
      (newArgs0.drop(2), Some(newArgs0(1)))
    } else {
      (newArgs0, None)
    }

    val (newArgs, eval) = if (newArgs1(0) == "--output-eval") {
      (newArgs0.drop(1), true)
    } else {
      (newArgs0, false)
    }

    val outputReport = outputPathOpt.getOrElse("args-report.html")

    val parsedSentencesPath = path.getOrElse("parsed_sentences_all.dat")

    implicit val sc = new SparkContext("local", "test-app")
    val startTime = System.nanoTime()

    val parsedSentences0 = Checkpointed(sc, parsedSentencesPath) {
      println("Rebuilding cache...")
      val sentencesFiles = sc.wholeTextFiles("zrobione131011/*/*/*.xml")
      val parsedSentences = sentencesFiles.flatMap { p => SkladnicaSentenceParser.parse(p._1, p._2)}
      parsedSentences.cache()
    }.collect()

    val ds = if(eval) {
      parsedSentences0.drop(parsedSentences0.length * 3 / 10)
    } else {
      // validation set
      parsedSentences0.take(parsedSentences0.length * 3 / 10)
    }

    val parsedSentences = sc.parallelize(ds, 100)
    println("sentences done!  ")


    val NUM = RulesAssigner.allRules.size
    val allAssigners = Seq.tabulate(NUM){ i => new RulesAssigner(i)}

    // Sentence, words, CRF labels (here taken from gold standard), gold standard assignment and labels
    // val sentencesWithArguments = parsedSentences.map { s => (s, s.words, extractArgumentTypeLabels(s), extractBothArgumentWithPredicate(s)) }
    val sentencesWithArguments = parsedSentences.map { s => (s, s.words, extractBothArgumentTypesAndAdjuncts(s), extractBothArgumentWithPredicate(s)) }
    val assignedResults = sentencesWithArguments.map {
      case (sentence, words, args, gold) =>
        val nearest = NearestPredicateAssigner.assign(words, args)
        val argNearest = ArgumentWiseNearestPredicate.assign(words, args)
        val rule = RulebasedPredicateAssigner.assign(words, args)
        val allResults = Seq.tabulate(NUM)( i=> s"rules:${"%02d".format(i)}" -> allAssigners(i).assign(words,args)).toMap
        (sentence, words, args, gold, allResults ++ Map("nearest" -> nearest, "argwise-nearest" -> argNearest, "rule" -> rule))
    }

    val htmlSentenceSnippets = assignedResults.map {
      case (sentence, words, args, gold, assignedArgs) =>
        val semHeads = SemanticHeadAssigner.assign(words, assignedArgs("rule"))

        PredicateAssigner.htmlReport(sentence, words, args, gold, assignedArgs, semHeads)
    }

    val colRes = assignedResults.collect
    val results = colRes.head._5.keys.toSeq.sorted.map { key =>
      val methodArguments = colRes.collect {
        case (sentence, words, args, gold, assignedArgs) if gold.flatMap(_.map(_._1)).distinct.size > 0 => (assignedArgs(key), gold, sentence)
      }
      key -> PredicateAssigner.calculateResults(methodArguments)
    }

    // per number of predicates
    val resPerPredCount = colRes.groupBy { case (sentence, words, args, gold, assignedArgs) =>
      gold.flatMap(_.map(_._1)).distinct.size
    }
    val accPerPredCnt = colRes.head._5.keys.toSeq.sorted.map { key =>
      key -> resPerPredCount.filter(_._1 > 0).map { kv =>
        val methodArgs = kv._2.map {case (sentence, words, args, gold, assignedArgs) =>
          (assignedArgs(key), gold, sentence)
        }
        kv._1 -> PredicateAssigner.calculateResults(methodArgs)
      }
    }

    for (methodResults <- accPerPredCnt) {
      println(s"------------- ${methodResults._1} by predictes no:")
      for (kv <- methodResults._2.toSeq.sorted) {
        println(s"\t${kv._1}: acc=${kv._2._1}  struct acc=${kv._2._2}  sent acc=${kv._2._3}  sents=${resPerPredCount(kv._1).size}")
      }
    }

    // per argument types
    {
      val argTypes = colRes.flatMap { case (sentence, words, args, gold, assignedArgs) =>
        gold.flatMap(_.map {
          x =>
            val at = x._2.argumentType
            if (at.contains("(")) {
              at.substring(0, at.indexOf('('))
            } else {
              at
            }
        })
      }.distinct
      val accPerType = colRes.head._5.keys.toSeq.sorted.map { key =>
        key ->  argTypes.map { at =>
           val methodArgs = colRes.map { case (sentence, words, args, gold, assignedArgs) =>
             val gf = gold.map(_.filter(_._2.argumentType.startsWith(at)))
             val aa = assignedArgs(key).map(_.filter(_._2.startsWith(at)))
             (aa, gf, sentence)
           }
           at -> PredicateAssigner.calculateResults(methodArgs)
        }
      }


      for (methodResults <- accPerType) {
        println(s"------------- ${methodResults._1} by arg type:")
        for (kv <- methodResults._2.toSeq.sorted) {
          println(s"\t${kv._1}: acc=${kv._2._1}   num=${kv._2._4}")
        }
      }
    }

    // per argument count
    {
      val argCounts = Seq.tabulate(15) { _ + 1}
      val accPerType = colRes.head._5.keys.toSeq.sorted.map { key =>
        key ->  argCounts.map { ac =>
          val methodArgs = colRes.map { case (sentence, words, args, gold, assignedArgs) =>
            val predsWithAC = gold.flatten.groupBy(_._1.left).mapValues(_.size).filter(_._2 == ac).map(_._1).toSet
            //println(predsWithAC)
            val gf = gold.map(_.filter(a => predsWithAC(a._1.left)))
            val aa = assignedArgs(key).map(_.filter(a => predsWithAC(a._1.left)))
            (aa, gf, sentence)
          }
          ac -> PredicateAssigner.calculateResults(methodArgs)
        }
      }


      for (methodResults <- accPerType) {
        println(s"------------- ${methodResults._1} by args no:")
        for (kv <- methodResults._2.toSeq.sorted) {
          println(s"\t${kv._1}: acc=${kv._2._1} num=${kv._2._4}")
        }
      }
    }


    println("results!")
    println(results)

    if (!eval) {
      val html = htmlSentenceSnippets.collect().mkString("<html><head><style>td { font-size: 14pt; font-weight: bold; }</style></head><body>", "", "</body></html>")
      val out = new PrintWriter(outputReport)
      out.write(html)
      out.close()
    }

    sc.stop()
  }
}
