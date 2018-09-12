package kodie.phd.formats

import kodie.phd.skladnica.types.{Word, Sentence}
import kodie.phd.assigner.{WordRulebasedPredicateAssigned, RulebasedPredicateAssigner}
import kodie.phd.skladnica.types
import java.io.PrintWriter
import scala.io.Source

/**
 * Created by kodie on 10/30/14.
 */
case class PPKey(verb: String, noun: String, prep: String, prepCase: String, subString : String = "_")
case class PPValue(verb: Double, noun: Double, decision: Int)

object PPAttachmentProbs {
  def load(inputPath: String) = {
    def stripPrefix(key: String) = key.substring(2)  // strips "0:" prefix
    def lineToInstanceValue(line: String) : Option[(PPKey, PPValue)] = {
      if (!line.trim.isEmpty) {
        val parts = line.split("\t")
        val key = PPKey(stripPrefix(parts(4)), stripPrefix(parts(5)), parts(6), parts(7))
        val value = PPValue(parts(0).toDouble, parts(1).toDouble, parts(2).toInt)
        Some(key -> value)
      } else {
        None
      }
    }
    Source.fromFile(inputPath).getLines().flatMap(lineToInstanceValue).toMap
  }

  def store(sentences: Traversable[Sentence], outputPath: String) = {
    val printer = new PrintWriter(outputPath)
    def printerVisitor(sentence: Sentence, word: Word, index: Int, governingNoun: Option[Word], governingPredicate: Option[Word]) {
      val pos = word.base
      val case_ = word.ctag.split(":")(1)
      val prepDependant = "_"
      val context = sentence.words.view(index - 5, index + 3).map(_.orth).mkString(" ")
      printer.print(s"0\t0:${governingPredicate.map(_.base).getOrElse("_")}\t0:${governingNoun.map(_.base).getOrElse("_")}\t$pos\t${case_}\t$prepDependant\t_\t($context)\n")
    }
    for (sentence <- sentences) {
      extract(sentence, printerVisitor)
    }
    printer.close()
  }

  def extract(sentence: Sentence, visitor: (Sentence, Word, Int, Option[Word], Option[Word]) => Unit) = {
    val verbMapping = WordRulebasedPredicateAssigned.assign(sentence.words, sentence.words.map(_ => "arg"))
    for ((word, index) <- sentence.words.zipWithIndex) {
      extractForWord(sentence, index, Some(verbMapping)).foreach {
        case (word, index, governingNoun, governingPredicate) => visitor(sentence, word, index, governingNoun, governingPredicate)
      }
    }
  }

  def extractForWord(sentence: Sentence, index: Int, verbMappingArg: Option[Seq[Option[(types.Span, String)]]] = None) : Option[(Word, Int, Option[Word], Option[Word])] = {
    val verbMapping = verbMappingArg.getOrElse {
      val args = (0 until sentence.words.size) map (i => if (i == index) "arg" else "_")
      WordRulebasedPredicateAssigned.assign(sentence.words, args)
    }
    val word = sentence.words(index)
    if (word.ctag.startsWith("prep")) {
      val governingNoun = findGoverningNoun(sentence, index)//.orElse(Some(Word("err", "err", "err")))
      val governingPredicate = findGoverningVerb(sentence, verbMapping, index)
      if (governingNoun.isDefined)
        return Some((word, index, governingNoun, governingPredicate))
    }
    return None
  }

  def findGoverningNoun(sentence: Sentence, index: Int) : Option[Word] = {
    def scan(filter: Word => Boolean, stop: Word => Boolean, direction: Int, limit: Int = 5) : Option[Word] = {
      val words = sentence.words

      for (i <- (index - 1) to (index - limit).max(0) by direction) {
        if (filter(words(i))) return Some(words(i))
        if (stop(words(i))) return None
      }
      return None
    }

    def scanToLeft(filter: Word => Boolean, stop: Word => Boolean, limit: Int = 5) = {
      scan(filter, stop, -1, limit)
    }

    def stopOnNonNominalOrAdjective(word: Word) = {
      !(word.ctag.startsWith("subst") || word.ctag.startsWith("psubst") || word.ctag.startsWith("adj") || word.ctag.startsWith("padj") || word.ctag.startsWith("ppron") || word.ctag.startsWith("depr"))
    }

    def lookForNonGenNoun(word: Word) = word.ctag.startsWith("subst") && !word.ctag.contains(":gen")

    def lookForAnyNounlikeNonGen(word: Word) = !word.ctag.contains(":gen") && (word.ctag.startsWith("subst") || word.ctag.startsWith("ppron") || word.ctag.startsWith("depr"))

    def lookForAnyNoun(word: Word) = word.ctag.startsWith("subst")

    def lookForAnyNounlike(word: Word) = (word.ctag.startsWith("subst") || word.ctag.startsWith("ppron") || word.ctag.startsWith("depr"))


    scanToLeft(lookForNonGenNoun _, stopOnNonNominalOrAdjective _).orElse {
      scanToLeft(lookForAnyNounlikeNonGen _, stopOnNonNominalOrAdjective _).orElse {
        scanToLeft(lookForAnyNoun _, stopOnNonNominalOrAdjective _).orElse {
          scanToLeft(lookForAnyNounlike _, stopOnNonNominalOrAdjective _)
        }
      }
    }

  }
  def findGoverningVerb(sentence: Sentence, assignments: Seq[Option[(types.Span, String)]], index: Int) : Option[Word] = {
    assignments(index).map(_._1).map(i => sentence.words(i.left))
  }

}
