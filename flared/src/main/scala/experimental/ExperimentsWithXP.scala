package experimental

import java.io.{FileOutputStream, FileInputStream, File, PrintWriter}
import java.nio.file.Paths

import kodie.phd.assigner.RulebasedPredicateAssigner
import kodie.phd.features._
import kodie.phd.formats.{CCLSentenceReader, PPAttachmentProbs, CCLSAXSentenceReader}
import kodie.phd.skladnica.features._
import kodie.phd.skladnica.types.{Sentence, Word}
import kodie.phd.tools.crf.{RemoteCRFDriver, CRF, LocalCRFDriver}
import kodie.phd.tools.wordnet.PLWordnet
import kodie.phd.walenty.old.{NewWalentySkladnicaBridge, WalentyRelizationsList, NewWalenty, WalentyArgument}
import kodie.phd.walenty.old.NewWalentySkladnicaBridge
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.SparkContext._

import scala.io.Source
import scala.collection.mutable

/** This file needs refactoring and cleanup. Check what is used where */
trait Feature[T] {
  def collect(in: T): Unit
  def levels: Int
  def labels: Seq[String]
  def valuesString(in: T): Seq[String] = values(in).map(_.toInt.toString)
  def collectingDone() {}
  def values(in: T): Seq[Double]
}

class CategoricalFeature[T](name: String, extractor: T => String) extends Feature[T] {
  val levels_ = mutable.ArrayBuffer[String]("_")
  val levelsSet = mutable.Set[String]()

  def collect(in: T) {
    val featureValue = extractor(in)
    if (!levelsSet.contains(featureValue)) {
      levels_ += featureValue
      levelsSet += featureValue
    }
  }

  def numCategories = levels_.size
  def levels = 1
  def labels = Seq(name)

  def values(in: T) = {
    val featureValue = extractor(in)
    Seq(if (levelsSet.contains(featureValue)) {
      levels_.indexOf(featureValue)
    } else {
      0
    })
  }

  override def valuesString(in: T) = {
    values(in).map(_.toInt).map(levels_.apply)
  }
}

object CategoricalFeature {
  def apply[T](name: String)(extractor: T => String) = new CategoricalFeature[T](name, extractor)
}

object BinarizedFeature {
  def apply[T](name: String)(extractor: T => Seq[String]) = new BinarizedFeature[T](name, extractor)
}

object MostFrequentBinarizedFeature {
  def apply[T](name: String)(n: Int)(collectFilter: T => Boolean)(extractor: T => Seq[String]) = {
    new MostFrequentBinarizedFeature[T](name, n, collectFilter, extractor)
  }
}

class MostFrequentBinarizedFeature[T](name: String, n: Int, collectFilter: T => Boolean, extractor: T => Seq[String]) extends Feature[T] {
  val levels_ = mutable.ArrayBuffer[String]("_")
  val levelsCounter = mutable.Map[String, Int]()
  var selectedFeatures: Option[Seq[String]] = None


  def collect(in: T) {
    if (collectFilter(in)) {
      val featureValues = extractor(in)
      for (featureValue <- featureValues) {
        if (levelsCounter.contains(featureValue)) {
          levelsCounter(featureValue) = levelsCounter.getOrElse(featureValue, 0) + 1
        } else {
          levels_ += featureValue
          levelsCounter(featureValue) = 1
        }
      }
    }
  }

  def levels = selectedFeatures.get.size
  def labels = selectedFeatures.get.map(s"$name:" + _)

  override def collectingDone {
    selectedFeatures = Some(levelsCounter.toSeq.sortBy(-_._2).map(_._1).take(n))
  }

  def values(in: T) = {
    val featureValues = extractor(in)
    Seq.tabulate(levels) { idx =>
      if (featureValues.contains(selectedFeatures.get.apply(idx))) 1.0
      else 0.0
    }
  }

}


class BinarizedFeature[T](name: String, extractor: T => Seq[String]) extends Feature[T] {
  val levels_ = mutable.ArrayBuffer[String]("_")
  val levelsSet = mutable.Set[String]()

  def collect(in: T) {
    val featureValues = extractor(in)
    for (featureValue <- featureValues) {
      if (!levelsSet.contains(featureValue)) {
        levels_ += featureValue
        levelsSet += featureValue
      }
    }
  }

  def levels = levels_.size
  def labels = levels_.map(s"$name:" + _)

  def values(in: T) = {
    val featureValues = extractor(in)
    Seq.tabulate(levels) { idx =>
      if (featureValues.contains(levels_(idx))) 1.0
      else 0.0
    }
  }

}

// TODO: remove
object ExperimentsWithXP extends App {
  implicit val sc = new SparkContext("local[24]", "test-app")
  // TODO: use local[40]
  val wordnet = PLWordnet.load()
  val walenty = new NewWalenty()

  type Input = ((String, String, Option[String], Option[Seq[String]], collection.Set[Int], collection.Set[String]), String)

  def getFeatures(words: Seq[Word], argReal: String) = {
    if (argReal.contains(" ")) {
      val parts = argReal.split(" ", 2)
      val prep = parts(0)
      val head = parts(1)
      val word = words.find(_.orth.equalsIgnoreCase(head))
      val wordBase = word.map(_.base)
      val wordCase = word.map(wordToCaseWord(_)).getOrElse("_" )
      val domains = wordBase.flatMap(wordnetDomains _)
      val topHypernyms = wordBase.map((wordnet.topSynsets _)).getOrElse(Set())
      val topHypernymsDomains = topHypernyms.flatMap(wordnetSynsetDomains _)

      //println(prep, wordCase, wordBase, domains, topHypernyms, topHypernymsDomains)
      Some((prep, wordCase, wordBase, domains, topHypernyms, topHypernymsDomains))
    } else {
      None
    }
  }

  def wordnetDomains(base: String): Option[Seq[String]] = {
    // println(wordnet.dictionary.get(base).map(_.map(wordnet.units.apply _).toList).toList)
    wordnet.dictionary.get(base).map(_.map(wordnet.units.apply _).map(_.domain))
  }

  def wordnetSynsetDomains(id: Int) = {
    val res = wordnet.synsets.get(id).map(synset => synset.lexUnits.map(wordnet.units.apply _).map(_.domain).toIndexedSeq)
    res.getOrElse(IndexedSeq())
  }
  def parseLocatedArgsFromAnn(ann: String) = {
    val regex = raw"([^\()]+) \(([^ ]+)\)".r
    regex.findAllIn(ann).matchData.map { matched =>
      matched.group(1).replace("!", "").trim() -> matched.group(2).trim()
    }
  }

  def findMatching(realizations: Seq[String], args: Seq[Option[Argument]]) = {
    val wal = WalentyArgument(IndexedSeq(), realizations.toIndexedSeq)
    val res = args.zipWithIndex.find( maybeArg =>
      // TODO: to źle działa z advp i realizacjami podanymi jako lemma (które chyba zresztą błędnie nie trafiają do
      // pola realizations
      maybeArg._1.filter(arg => NewWalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty(arg.argumentType, wal)) != None)
    val r = res.flatMap(x => x._1.map(y => x._2 -> y))
    r
  }

  def parseSingleArg(argStr: String) = {
    val start = argStr.indexOf('<')
    val end = argStr.indexOf('>')
    argStr.substring(start+1, end)
  }

  def realizedArgumentsFromAnnotation(ann: Map[String, String]) = {
    val argsAnn = ann("wybor argumentow")
    argsAnn.split('+').map(a => parseSingleArg(a.trim()))
  }

  def parseLemma(lemmaArg: String) = {
    val parts = lemmaArg.trim.substring(6,lemmaArg.trim.length - 1).split(",")
    if (parts(1).trim == "")  (parts(0).trim, parts(2).trim)
    else (parts(0).trim + " " + parts(1).trim, parts(2).trim)
  }

  val dataset = CSVToXCES.rows.map { walentyExample =>
    val words = CSVToXCES.toSentence(walentyExample("przyklad"), walentyExample("fragmenty przykladu")).words
    val tomekArgs = parseLocatedArgsFromAnn(walentyExample("otagowany przyklad")).toIndexedSeq
    val lemma = tomekArgs.filter(_._2.contains("lemma")).map(x => parseLemma(x._2))
    val possibleArgs = lemma.flatMap(p => walenty.framesFor(p._1, p._2)).flatten.flatMap(_.realizations).filter(_.contains("xp("))
    val tomekXpArgs = tomekArgs.filter(_._2.contains("xp("))
    val tomekXpArgsFeatures = tomekXpArgs.map(arg => getFeatures(words, arg._1) -> arg._2).filter(_._1 != None).map(x => x._1.get -> x._2)
    val tomekPrepArgs = tomekArgs.filter(_._2.contains("prep"))
    val tomekPrepArgsFeatures = tomekPrepArgs.map(arg => getFeatures(words, arg._1) -> arg._2).filter(_._1 != None).map(x => x._1.get -> x._2)
    println(lemma, possibleArgs, possibleArgs.find(_.contains("xp(")) != None, tomekPrepArgs, if (possibleArgs.find(_.contains("xp(")) != None) tomekPrepArgsFeatures else IndexedSeq())
    (tomekXpArgsFeatures, if (possibleArgs.find(_.contains("xp(")) != None) tomekPrepArgsFeatures else IndexedSeq())
  }

  val xpExamples = dataset.flatMap(_._1).toIndexedSeq
  val prepExamples = dataset.flatMap(_._2).toIndexedSeq

  println("xp", xpExamples.size)
  println("prep", prepExamples.size)

  val otherExamples = prepExamples.map(x => x._1 -> "other")

  val allExamples = xpExamples ++ otherExamples

  val features: Seq[Feature[Input]] = Seq(
    CategoricalFeature[Input]("prep"){ in: Input => in._1._1 },
    CategoricalFeature[Input]("case"){ in: Input => in._1._2 },
    BinarizedFeature[Input]("domains"){ in: Input => in._1._4.getOrElse(Seq()) },
    MostFrequentBinarizedFeature[Input]("topSynsets") (16) ((in: Input) => in._2.contains("xp")) {
      in: Input => in._1._5.map(_.toString).toSeq
    },
    BinarizedFeature[Input]("topDomains"){ in: Input => in._1._6.toSeq },
    CategoricalFeature[Input]("class"){ in: Input => in._2 }
  )

  // TODO: in fact this should be already only train part!
  for (instance <- allExamples) {
    for (feat <- features) {
      feat.collect(instance)
    }
  }

  for (feat <- features) {
    feat.collectingDone
  }


  println("here!")
  println(features.last.asInstanceOf[CategoricalFeature[Input]].levels_)
  val vecSize = features.map(_.levels).sum
  println("vectorSize", vecSize)

  val featuresWithoutClass = features.dropRight(1)
  val classFeature = features.last

  val codedDataset = for (instance <- allExamples) yield {
    val codedInstance: Seq[Double] = featuresWithoutClass.flatMap(_.values(instance))
    val classFeat = classFeature.values(instance).head
    LabeledPoint(classFeat, Vectors.dense(codedInstance.toArray))
  }

  val rddCodedDataset = sc.parallelize(codedDataset).cache()

  val output = new FileOutputStream(("encoded.csv"))
  output.write(("wordBase," + features.flatMap(_.labels).mkString(",") + "\n") getBytes)
  for (instance <- allExamples) yield {
    val codedInstance: Seq[String] = featuresWithoutClass.flatMap(_.valuesString(instance))
    val classFeat = classFeature.valuesString(instance).head
    val base = instance._1._3.getOrElse("_")
    //val lemma = tomekArgs.filter(_._2.contains("lemma")).map(x => parseLemma(x._2))


    val rows = Seq(base) ++ codedInstance ++ Seq(classFeat)
    output.write((rows.mkString(",") + "\n").getBytes)
  }
  output.close()

   // testRandomForests(rddCodedDataset)

  def testRandomForests(data: RDD[LabeledPoint]) = {
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainingData, testData) = (splits(0), splits(1))
    println(s"training data: ${trainingData.count()}")

    // Train a RandomForest model.
    //  Empty categoricalFeaturesInfo indicates all features are continuous.
    val numClasses = features.last.asInstanceOf[CategoricalFeature[Input]].numCategories
    val categoricalFeaturesInfo = Map[Int, Int](
      0 -> features(0).asInstanceOf[CategoricalFeature[Input]].numCategories,
      1 -> features(1).asInstanceOf[CategoricalFeature[Input]].numCategories
    )
    val numTrees = 2048  // Use more in practice.
    val featureSubsetStrategy = "auto" // Let the algorithm choose.
    val impurity = "gini"
    val maxDepth = 16
    val maxBins = 64

    val model = RandomForest.trainClassifier(trainingData, numClasses, categoricalFeaturesInfo,
      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

    // Evaluate model on test instances and compute test error
    val labelAndPreds = testData.map { point =>
      val prediction = model.predict(point.features)
      (point.label, prediction)
    }
    val testErr = labelAndPreds.filter(r => r._1 != r._2).count.toDouble / testData.count()
    println("Test Error = " + testErr)


  }


}

// TODO: remove
object OldExperimentsWithXP extends App {
  implicit val sc = new SparkContext("local[1]", "test-app")
  // TODO: use local[40]
  val inputPath = args.lift(1).getOrElse("walentyExamples-with-args-and-frame.obj")

  // TODO: ten plik jest walnienty i nie ma wszystkich danych, bo joinowanie nie działa :)
  val data = sc.objectFile[(String, ((Sentence, Seq[Option[Argument]]), Map[String, String]))](inputPath).cache()

  val top = data.take(100).map(_._2)

  val xpRealizations = new WalentyRelizationsList()

  val wordnet = PLWordnet.load()

  def wordnetDomains(base: String): Option[Seq[String]] = {
    // println(wordnet.dictionary.get(base).map(_.map(wordnet.units.apply _).toList).toList)
    wordnet.dictionary.get(base).map(_.map(wordnet.units.apply _).map(_.domain))
  }

  def wordnetSynsetDomains(id: Int) = {
    val res = wordnet.synsets.get(id).map(synset => synset.lexUnits.map(wordnet.units.apply _).map(_.domain).toIndexedSeq)
    res.getOrElse(IndexedSeq())
  }

  //wordnet.
 // println(xpRealizations.realizations.head)
 // println(xpRealizations.skladnicaPossibilities.head)

  val dataset = data.map(_._2).map { s =>
    val args = realizedArgumentsFromAnnotation(s._2).toList
    val perf = if (s._2("ramka").contains("imperf")) "imperf" else "perf"
    val xpargs = args.filter(_.contains("xp("))
    val pred = s._2("haslo")
    val frame = s._2("ramka")
    val words = s._1._1.words
    val realizations = xpargs.map { xp =>
      val reals = xpRealizations.realizations.get(xp)
      val viableArgs = s._1._2.map(maybeArg => maybeArg.filter(arg => words(arg.predicate.left).base == pred && words(arg.predicate.left).ctag.contains(s":$perf")))
      xp -> reals.map(r => findMatching(r, viableArgs)).getOrElse(None)
    }
    val printRealizations = realizations.map { rr =>
      val (xparg, maybeAnswer) = rr

      val lbl = maybeAnswer.map {
        case (idx, arg) =>
          words(idx).orth + arg.semanticHead.filter(_ != idx).map(i => " " + words(i).orth).getOrElse("")
      }

      s"$lbl ($xparg)"
    }

    def getFeatures(argReal: String) = {
      if (argReal.contains(" ")) {
        val parts = argReal.split(" ", 2)
        println(argReal, parts)
        val prep = parts(0)
        val head = parts(1)
        val word = words.find(_.orth.equalsIgnoreCase(head))
        val wordBase = word.map(_.base)
        val wordCase = word.map(wordToCaseWord(_)).getOrElse("_" )
        val domains = wordBase.flatMap(wordnetDomains _)
        val topHypernyms = wordBase.map((wordnet.topSynsets _)).getOrElse(Set())
        val topHypernymsDomains = topHypernyms.flatMap(wordnetSynsetDomains _)

        //println(prep, wordCase, wordBase, domains, topHypernyms, topHypernymsDomains)
        Some((prep, wordCase, wordBase, domains, topHypernyms, topHypernymsDomains))
      } else {
        None
      }
    }

    val tomekArgs = parseLocatedArgsFromAnn(s._2("otagowany przyklad")).toIndexedSeq
    val tomekXpArgs = tomekArgs.filter(_._2.contains("xp("))
    val tomekXpArgsFeatures = tomekXpArgs.map(arg => getFeatures(arg._1) -> arg._2).filter(_._1 != None).map(x => x._1.get -> x._2)
    val tomekPrepArgs = tomekArgs.filter(_._2.contains("prep"))
    val tomekPrepArgsFeatures = tomekPrepArgs.map(arg => getFeatures(arg._1) -> arg._2).filter(_._1 != None).map(x => x._1.get -> x._2)

    println(pred + " " + perf)
    //println(frame)
    // println(perf)
    //println(xpargs)
    println(printRealizations)
    println(tomekPrepArgsFeatures)
    println(tomekXpArgsFeatures)
    //println(tomekArgs.toList)
    println(s._1._1.text)
    //println(s._1._2.flatMap(x => x))
    (tomekXpArgsFeatures, tomekPrepArgsFeatures)
  }

  val materialized = dataset.collect()
  val xpExamples = materialized.flatMap(_._1).toIndexedSeq
  val prepExamples = materialized.flatMap(_._2).toIndexedSeq

  println(dataset.count())
  println(xpExamples.size)
  println(prepExamples.size)

  def parseLocatedArgsFromAnn(ann: String) = {
    val regex = raw"([^\()]+) \(([^ ]+)\)".r
    regex.findAllIn(ann).matchData.map { matched =>
      matched.group(1).replace("!", "").trim() -> matched.group(2).trim()
    }
  }

  def findMatching(realizations: Seq[String], args: Seq[Option[Argument]]) = {
    val wal = WalentyArgument(IndexedSeq(), realizations.toIndexedSeq)
    val res = args.zipWithIndex.find( maybeArg =>
      // TODO: to źle działa z advp i realizacjami podanymi jako lemma (które chyba zresztą błędnie nie trafiają do
      // pola realizations
      maybeArg._1.filter(arg => NewWalentySkladnicaBridge.tryToMatchSkladnicaAndWalenty(arg.argumentType, wal)) != None)
    val r = res.flatMap(x => x._1.map(y => x._2 -> y))
    r
  }

  def parseSingleArg(argStr: String) = {
    val start = argStr.indexOf('<')
    val end = argStr.indexOf('>')
    argStr.substring(start+1, end)
  }

  def realizedArgumentsFromAnnotation(ann: Map[String, String]) = {
    val argsAnn = ann("wybor argumentow")
    argsAnn.split('+').map(a => parseSingleArg(a.trim()))
  }

}

object CheckWalentyAnnotationToXP extends App {
  implicit val sc = new SparkContext("local[1]", "test-app")

  val inputPath = args.lift(1).getOrElse("walentyExamples-with-args-and-ann.obj")

  val data = sc.objectFile[((Sentence, Seq[Option[Argument]]), Map[String, String])](inputPath).cache()

  data.foreach {
    case ((sentence, args), sampleAnnotation) =>
      if (sentence.text.trim() != sampleAnnotation("przyklad").trim()) {
        println("ok!")
        println(sentence.text)
        println(sampleAnnotation("przyklad"))
      }
  }

}

object AnnotateSentencesFromXP extends App {
  val config = new SparkConf().setMaster("local[20]").setAppName("test-app").set("spark.executor.memory", "44g")
  val outputPath = args.lift(1).getOrElse("data/pantera-new-multi-with-cp-heur-walentyExamples-with-args-and-ann.obj")

  val sshCRFDriver = LocalCRFDriver

  val modelPath = Paths.get("data/pantera-final-model.crf").toAbsolutePath.toString
  val assigner = RulebasedPredicateAssigner
  assigner.keepNotConnected = true


  val sents = CCLSAXSentenceReader.read("walentyExamples.xces.chunked", new FileInputStream("data/pantera-walenty-examples.txt.disamb.chunked"))
  // Filtering out "Bład tagowania" sentences (that have no words!)
  // czy na pewno kolejnosc jest w porządku?
  val sentsWithRows = sents.zip(CSVToXCES.rows).filterNot(_._1.words.isEmpty)

  def onlyChars(s: String) = s.replace("&quot;", "\"").replaceAll("[^a-zA-Z0-9]", "")
    sentsWithRows.foreach { case (sent, row) =>
      if (onlyChars(sent.text) != onlyChars(row("przyklad"))) {
        println("Mismatch: ")
        println("\t" + sent.text)
        println("\t" + row("przyklad"))
      }
  }

  implicit val sc = new SparkContext(config)
  val files = sc.parallelize(List("walentyExamples.xces.chunked"))

  val sparkSentencesWithRows = sc.parallelize(sentsWithRows)

  val ppCases = PPAttachmentProbs.load("pp.in")
  val featureExtractors: Seq[FeatureExtractor[(Sentence, Int)]] = Seq(wordToOrth _, wordToBase _, wordToPoS _, wordToCase _, wordToAspect _, wordToChunkHead _, predNumberMatch _, ppAttachmentNoun(ppCases, 0.01) _, predDistance("pp", Seq("prep")) _, predDistance("subst", Seq("subst", "psubst")) _, matchingCaseNearby _) map (x => Function.tupled(x))
  val conllWriter = new SkladnicaConllWriter(featureExtractors, extractNoArguments)

  val crf = new CRF(modelPath, sshCRFDriver)

  val sentenceWithArguments = sparkSentencesWithRows.filter(_._1.words.size <= 100).mapPartitions {
    case (sentencesWithAnn) =>
      val sentWithAnnSeq = sentencesWithAnn.toIndexedSeq
      val extras = sentWithAnnSeq.map(_._2)
      //println(sentWithAnnSeq.size)
      MultiLabelSparkHelper.extractFromSentences(sentWithAnnSeq.map(_._1), crf, conllWriter, assigner, 3, 0.6).iterator.zip(extras.iterator)
  }

  sentenceWithArguments.saveAsObjectFile(outputPath)
}

object CSVToXCES extends BaseCSVToXCES("data/walenty_detailed_examples_20160205.csv")

class BaseCSVToXCES(input: String) /*extends App*/{
  val lines = Source.fromFile(input, "UTF-8").getLines()
  val headerLine = lines.next()
  val rows = mutable.ArrayBuffer[collection.Map[String, String]]()

  val columns = headerLine.split("\t")
  for (row <- lines) {
    val values = row.split("\t")
    val dict = columns.iterator.zip(values.iterator).toMap
    rows += dict
  }

  val reArgs = raw"([^\(]+) \(([^ ]+)\)".r
  def parseArgs(value: String) = {
    reArgs.findAllMatchIn(value).map { matchingPart => matchingPart.group(1).trim() -> matchingPart.group(2).trim() }.toIndexedSeq
  }

  lazy val wordAndType = rows.flatMap(row => parseArgs(row("otagowany przyklad")))
  lazy val onlyXps = wordAndType.filter(_._2.contains("xp("))
  lazy val groupped = onlyXps.groupBy(_._2).mapValues(_.map(_._1)).toMap

  lazy val dataset = rows.map(x => toSentence(x("przyklad"), x("fragmenty przykladu")))

  def toSentence(text: String, taggedSentence: String): Sentence = {
    val wordsPlain = taggedSentence.split(" ").map(_.trim())
    val words = wordsPlain.filterNot(_.trim().isEmpty).map { word =>
      if (word.startsWith("[")) {
        Word("[", "[", "interp")
      } else if (word.startsWith(">")) {
        Word("&gt;", "&gt;", "interp")
      } else {
        val parts = word.split('[')
        //println(word, parts.toList)
        val orth = parts(0).replace("&", "&amp;")
        val base = parts(1).split('>')(0).replace("&", "&amp;")
        val ctag = parts(1).split('>')(1).replace("]", "").replace("xxx", "ign")

        Word(orth, base, ctag)
      }
    }
    Sentence("csv-file", text, words, Map(), Map())
  }

  def outputToXCES(out: PrintWriter, dataset: Seq[Sentence]) = {
    def replaceEntities(text: String): String = {
      text.replace("<", "&gt;").replace(">", "&lt;")
    }
    out.write(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<!DOCTYPE cesAna SYSTEM "xcesAnaIPI.dtd">
        |<cesAna version="1.0" type="lex disamb">
        |<chunkList>
      """.stripMargin)

    for (sentence <- dataset) {
      out.write("""<chunk type="p"><chunk type="s">""" + "\n")
      for (word <- sentence.words) {
        out.write("<tok>\n")
        out.write(s"<orth>${replaceEntities(word.orth)}</orth>\n")
        out.write("<lex disamb=\"1\">" +  s"<base>${replaceEntities(word.base)}</base><ctag>${word.ctag}</ctag></lex>\n")
        out.write("</tok>")
      }
      out.write("</chunk></chunk>")
    }


    out.write("""
                |</chunkList>
                |</cesAna>""".stripMargin)
  }
}
