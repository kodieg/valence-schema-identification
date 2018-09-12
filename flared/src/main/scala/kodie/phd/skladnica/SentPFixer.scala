package kodie.phd.skladnica

import kodie.phd.skladnica.types.{Index, Sentence, Span, Word}
import kodie.phd.walenty.NCPMatcher

/**
  sentp(pz) have invalid heads due to bug in a skladnica parser
 */
object SentPFixer {
  def fixSentP(sentence: Sentence) = {
    val QUESTION_WORDS = NCPMatcher.QUESTION_WORDS.toSet

    def isQuestionWord(word: Word) = QUESTION_WORDS(word.base.toLowerCase) || QUESTION_WORDS(word.orth.toLowerCase)

    def isVerb(word: Word) = {
      val VERB_POS = Set("inf", "praet", "fin",  "impt", "imps", "bedzie", "pred", "winien"/*,"pcon"*/ /*, "pant"*, "ger", "ppas"*/)
      VERB_POS.exists(word.ctag.startsWith)
    }

    def isNotQuestionWord(word: Word) = {
      // synt head of sentp(pz) is set to some predicate on the right of the Question Word
      isVerb(word) || word.ctag.startsWith("subst")
    }


    def findQuestionWordOnTheLeft(_idx: Int): Option[Int] = {
      var idx = _idx
      while (idx >= 0) {
        if (isQuestionWord(sentence.words(idx))) return Some(idx)
        idx -= 1
      }
      return None
    }

    def fixIn(argsContainer: Map[Span, Seq[types.Argument]]) = {
      argsContainer.map { case (span, args) =>
        span -> args.map { arg =>
          val idx = arg.syntacticHead.map(_.i).getOrElse(sentence.words.size - 1)
          if (arg.argumentType == "sentp(pz)" && isNotQuestionWord(sentence.words(idx))) {
            val qw = findQuestionWordOnTheLeft(idx)
            qw.map { i => arg.copy(syntacticHead = Some(Index(i)))}.getOrElse(arg)
          } else {
            arg
          }
        }
      }
    }

    val as = fixIn(sentence.argumentStructures)
    val fp = fixIn(sentence.freePhrases)

    val sentpIndexes = (fp.values ++ as.values).flatMap(_.collect {
      case arg if arg.argumentType == "sentp(pz)" => arg.syntacticHead.map(_.i)
    }).flatten.toSet

    sentence.copy (
      argumentStructures = as.mapValues(_.filterNot(arg => arg.argumentType != "sentp(pz)" && sentpIndexes(arg.syntacticHead.map(_.i).getOrElse(-1)))).map(identity _),
      freePhrases = fp.mapValues(_.filterNot(arg => arg.argumentType != "sentp(pz)" && sentpIndexes(arg.syntacticHead.map(_.i).getOrElse(-1)))).map(identity _)
    )
  }
}
