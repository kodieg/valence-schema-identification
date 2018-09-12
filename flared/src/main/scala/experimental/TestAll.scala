package experimental

import kodie.phd.skladnica.{SentPFixer, SkladnicaSentenceParser}
import org.apache.spark.SparkContext
import scala.xml.{NodeSeq, XML}
import kodie.phd.formats._
import kodie.phd.skladnica.features._
import kodie.phd.tools.crf.{LocalCRFDriver, RemoteCRFDriver, CRF}
import kodie.phd.utils.spark.Checkpointed
import kodie.phd.utils.ml.DatasetUtils
import kodie.phd.assigner._
import java.io.{FileInputStream, FileOutputStream, PrintWriter}
import kodie.phd.features._
import scala.Some
import kodie.phd.skladnica.types.Sentence
import org.apache.spark.rdd.RDD
import scala.Some
import kodie.phd.skladnica.types.Sentence
import scala.Some
import kodie.phd.skladnica.types.Sentence

/*
Not sure what's that/. Is this actually feature evaluation?
 */
object MainTestAll {

  def main(args: Array[String]) {
    val sshCRFDriver = LocalCRFDriver // new RemoteCRFDriver("kodie@mozart")
    val parsedSentencesPath = "parsed_sentences_all.dat"
    val chunkedSentencesPath = "data/Skladnica-full-pantera.dat"  // "chunked_sentences.dat"

    implicit val sc = new SparkContext("local", "test-app")
    val startTime = System.nanoTime()

    val parsedSentences = Checkpointed(sc, parsedSentencesPath) {
      println("Rebuilding cache...")
      val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/*/*/*.xml")
      // val sentencesFiles = sc.wholeTextFiles("/Users/kodie/Downloads/zrobione131011/NKJP_1M_EkspressWieczorny/*/*.xml")
      val parsedSentences = sentencesFiles.flatMap { p => SkladnicaSentenceParser.parse(p._1, p._2) }
      parsedSentences.cache()
    }
    println("sentences done!  ")

    //CCLWriter.write(parsedSentences.collect(), new PrintWriter(new FileOutputStream("corpus.ccl.xml")))
    val chunkedSentences: RDD[Sentence] = Checkpointed(sc, chunkedSentencesPath) {
      val chunks = CCLReader.read(new FileInputStream("corpus.ccl.xml.chunked"))
      val sents = parsedSentences.map {
        sentence =>
          sentence.copy(chunks = Some(chunks(sentence.source).toSeq))
      }
      sents.cache()
      sents
    }.map(SentPFixer.fixSentP _)

    /*val sents = chunkedSentences.collect()
    PPAttachmentProbs.store(sents, "pp.data")
    System.exit(0)
    */

    // Sentence, words, CRF labels (here taken from gold standard), gold standard assignment and labels
    // val sentencesWithArguments = parsedSentences.map { s => (s, s.words, extractArgumentTypeLabels(s), extractBothArgumentWithPredicate(s)) }
    /*val sentencesWithArguments = parsedSentences.map { s => (s, s.words, extractBothArgumentTypesAndAdjuncts(s), extractBothArgumentWithPredicate(s)) }
    val assignedResults = sentencesWithArguments.map {
      case (sentence, words, args, gold) =>
        val nearest = NearestPredicateAssigner.assign(words, args)
        val argNearest = ArgumentWiseNearestPredicate.assign(words, args)
        val rule = RulebasedPredicateAssigner.assign(words, args)
        (sentence, words, args, gold, Map(/*"nearest" -> nearest, "argwise-nearest" -> argNearest,*/ "rule" -> rule))
    }

    val htmlSentenceSnippets = assignedResults.map {
      case (sentence, words, args, gold, assignedArgs) =>
        val semHeads = SemanticHeadAssigner.assign(words, assignedArgs("rule"))

        PredicateAssigner.htmlReport(sentence, words, args, gold, assignedArgs, semHeads)
    }

    val results = Seq("rule").map { key =>
      val methodArguments = assignedResults.collect {
        case (sentence, words, args, gold, assignedArgs) if gold.flatMap(_.map(_._1)).distinct.size > 0 => (assignedArgs(key), gold, sentence)
      }
      key -> PredicateAssigner.calculateResults(methodArguments.collect())
    }


    println("results!")
    println(results)

    val html = htmlSentenceSnippets.collect().mkString("<html><head><style>td { font-size: 14pt; font-weight: bold; }</style></head><body>", "", "</body></html>")
    val out = new PrintWriter("/Users/kodie/args-report.html")
    out.write(html)
    out.close()

    sc.stop()
    System.exit(0)*/


    // TODO: calculateResults is simplicistic and assume correctnes of argument possition
    /*val filtered = assignedResults.filter(x => x._2 != x._1).collect()*/
    /*for (sentence <- filtered) {
        println(sentence._4.words.map(_.orth).zip(sentence._5))
        println(sentence._1.flatMap(x => x))
        println(sentence._2.flatMap(x => x))
        println(sentence._3.flatMap(_.map(x => x._1 -> (x._2.syntacticHead, x._2.argumentType))))

    }*/
    /*val assignedResults2 = assignedResults.map(x => (x._1, x._3, x._4))
    val results = PredicateAssigner.calculateResults(assignedResults2.collect())
    println(results)
*/
   //System.exit(0)


    val ppCases = PPAttachmentProbs.load("pp.in")



    val featureExtractors: Seq[FeatureExtractor[(Sentence, Int)]] = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _, wordToAspect _, wordToChunkHead _, predNumberMatch _, ppAttachmentNoun(ppCases, 0.01) _, predDistance("pp", Seq("prep")) _, predDistance("subst", Seq("subst", "psubst")) _, matchingCaseNearby _) map (x => Function.tupled(x))
    val anyArgExtractor = ((sentence: Sentence) => extractBothArgumentTypesAndAdjuncts(sentence).map(s => if (s == "_") s else "ARG"))
    val conllWriter = new SkladnicaConllWriter(featureExtractors, extractBothArgumentTypesAndAdjuncts)
    // val conllWriter = new SkladnicaConllWriter(featureExtractors, anyArgExtractor)
    val conllDataset = chunkedSentences.map(s => (s, conllWriter.extract(s)))
    val localDataset = conllDataset.collect()

    CRF.storeDataset("conll-all", localDataset.map(_._2))

    val foldedDataset = DatasetUtils.folds(10, localDataset)

    val featuresSets = FeaturesSets.allWithB
    val out = new PrintWriter("reuslts-with-B-m1000-f10-c1.0-eta0.0001.txt")

    for ((featSetName, feats) <- featuresSets) {

      println("\n" + featSetName)
      out.println("\n" + featSetName)

      val crfResults = foldedDataset.map {
        case (train, test) =>
          val model = CRF.train(feats, train.map(_._2), sshCRFDriver)
          val predictionResults = model.predict(test.map(_._2))
          val stats = ConllReader.calculateStats(predictionResults)
          val predictedArgumentsPerSentence = ConllReader.bySentence(predictionResults).map(_.map(_.last))
          val testWithArgs = test.map(_._1).zip(predictedArgumentsPerSentence)
          val assignStats = PredicateAssigner.evaluate(RulebasedPredicateAssigner, testWithArgs)

          println(stats, assignStats)
          Map("recall+precision" -> stats, "assignmnetStats" -> assignStats)
      }
      for (line <- crfResults) {
        println(line)
        out.println(line)
      }
      out.flush()
    }
    out.close()

//    val model = CRF.train(features, localDataset, sshCRFDriver)
//    val predictionResults = model.predict(localDataset)
//    va  l classesBySentence = sc.parallelize(ConllReader.extractPredictedClasses(predictionResults))
//    val sentencesWithClasses = parsedSentences.zip(classesBySentence)
//    println(ConllReader.calculateStats(predictionResults))
    // println(results.toList)
    //println(conllDataset.collect().map(_.toList).toList)


    /*{
      case (name, content) =>
        val root = XML.loadString(content)

        if ((root \\ "base-answer" \ "@type" text) == "FULL") {

          val argumentsStructures = SkladnicaSentenceParser.findArgumentStructure(root)
          val argumentsTypesFromTags = SkladnicaSentenceParser.findArgumentTypesForPredicates(root)

          val extractedArgumentTypes = argumentsStructures.map(s => s._1 -> s._2.map(_.argumentType).toList.sorted)
          if (extractedArgumentTypes != argumentsTypesFromTags) {
            println(s"$extractedArgumentTypes did not match $argumentsTypesFromTags in $name")
            None
          } else {
            Some(name)
          }
        } else {
          None
        }
    }
    println(parsedSentences.collect().toList)
    */
  }
}

object FeaturesSets {
  val orth = """
  |U00:%x[-2,1]
  |U01:%x[-1,1]
  |U02:%x[0,1]
  |U03:%x[1,1]
  |U04:%x[2,1]
  |#U05:%x[-1,1]/%x[0,1]
  |#U06:%x[0,1]/%x[1,1]
  |""".stripMargin

  val base = """U10:%x[-2,2]
  |U11:%x[-1,2]
  |U12:%x[0,2]
  |U13:%x[1,2]
  |U14:%x[2,2]
  |#U15:%x[-2,2]/%x[-1,2]
  |#U16:%x[-1,2]/%x[0,2]
  |#U17:%x[0,2]/%x[1,2]
  |#U18:%x[1,2]/%x[2,2]
  |""".stripMargin

  val pos = """U20:%x[-2,2]/%x[-1,2]/%x[0,2]
  |U21:%x[-1,2]/%x[0,2]/%x[1,2]
  |U22:%x[0,2]/%x[1,2]/%x[2,2]
  |""".stripMargin

  val caseF = """U30:%x[0,3]
  |U31:%x[0,2]/%x[0,3]
  |U32:%x[-1,3]
  |U33:%x[1,3]
  |""".stripMargin

  val aspect = "U40:%x[0,4]\n"
  val chunkHead = """U50:%x[0,5]
                    |U51:%x[-1,5]""".stripMargin

  val predNumberMatch = "U60:%x[0,6]\n"

  val ppAttachment = """U70:%x[0,7]
  |U71:%x[0,7]/%x[0,2]
  |#U72:%x[0,7]/%x[0,2]/%x[0,3]
  |""".stripMargin
  val predDistPP = """U80:%x[0,8]
  |U81:%x[0,8]/%x[0,2]
  |#U82:%x[0,8]/%x[0,2]/%x[0,3]
  |""".stripMargin

  val predDistSubst = """U90:%x[0,9]
  |U91:%x[0,9]/%x[0,2]
  |#U92:%x[0,9]/%x[0,2]/%x[0,3]
  |""".stripMargin

  val matchingCaseNearby = """U100:%x[0,10]
  |U101:%x[0,10]/%x[0,2]
  |U102:%x[0,10]/%x[0,3]
  |#U103:%x[0,10]/%x[0,2]/%x[0,3]
  |#U104:%x[0,10]/%x[0,5]
  |#U105:%x[-1,10]/%x[-1,5]
  |#U106:%x[-1,10]
  |#U107:%x[-2,10]
  """.stripMargin

  val names = "orth, base, pos, caseF, aspect, chunkHead, predNumberMatch, ppAttachment, predDistPP, predDistSubst, matchingCaseNearby".split(", ")
  val fsets = Seq(orth, base, pos, caseF, aspect, chunkHead, predNumberMatch, ppAttachment, predDistPP, predDistSubst, matchingCaseNearby)

  val all = (3 to fsets.size) map { n =>
    names.take(n).mkString(" + ") -> fsets.take(n).mkString("\n")
  }

  val allWithB = all.map(x => x._1 -> (x._2 + "\nB\n"))
}
