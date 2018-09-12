package kodie.phd.skladnica

import kodie.phd.features._
import kodie.phd.formats.{PPAttachmentProbs, PPKey, PPValue, ConllWriter}
import kodie.phd.skladnica.types._
import kodie.phd.assigner.WordRulebasedPredicateAssigned

/**
 * Created by kodie on 9/14/14.
 */
package object features {
  def sanitizeFeature(s: String) = {
    val v = s.replace(" ", "").replace("\n", "").replace("\t", "")
    if (v.trim().isEmpty) "_"
    else v.trim()
  }
  def wordToOrth(sentence: Sentence, index: Int) = sanitizeFeature(sentence.words(index).orth)

  def wordToBase(sentence: Sentence, index: Int) = sanitizeFeature(sentence.words(index).base)

  def wordToPoS(sentence: Sentence, index: Int) = sanitizeFeature(sentence.words(index).ctag.split(":", 2)(0))

  def wordToCase(sentence: Sentence, index: Int) : String = {
    wordToCaseWord(sentence.words(index))
  }

  def wordToCaseWord(word: Word) : String = {
    val parts = word.ctag.split(":")
    val pos = parts(0)

    if (List("subst", "ger", "num", "psubst", "adj", "padj", "ppron12", "ppron3").contains(pos))
      parts(2)
    else if (List("siebie", "prep").contains(pos))
      parts(1)
    else
      "_"
  }

  def wordToAspect(sentence: Sentence, index: Int) = {
    val parts = sentence.words(index).ctag.split(":")
    val pos = parts(0)

    if (pos == "inf") parts(1)
    else "_"
  }

  def wordToChunkHead(sentence: Sentence, index: Int) : String = {
    sentence.chunks.flatMap {
      case chunks if (chunks.flatMap(_.head).map(_.i).contains(index)) => Some("chunk_head")
      case _ => None
    }.getOrElse("_")
  }

  def predNumberMatch(sentence: Sentence, index: Int) : String = {
    val args = (0 until sentence.words.size) map (i => if (index == i) "arg" else "_")
    val assigned = WordRulebasedPredicateAssigned.assign(sentence.words, args)
    assigned(index).map { pred =>
      val predIndex = pred._1.left
      val predParts = sentence.words(predIndex).ctag.split(":")
      val wordParts = sentence.words(index).ctag.split(":")
      if (predParts.size > 1 && wordParts.size > 1) {
        val predNum = predParts(1)
        val wordNum = wordParts(1)

        if (predNum == wordNum) predNum
        else "_"
      } else "_"
    }.getOrElse("_")
  }

  def predDistance(name: String, includedPosPrefixes: Seq[String])(sentence: Sentence, index: Int) : String = {
    val args = (0 until sentence.words.size) map (i => if (index == i) "arg" else "_")
    val assigned = WordRulebasedPredicateAssigned.assign(sentence.words, args)
    def anyPrefix(ctag: String) = {
      includedPosPrefixes.exists(prefix => ctag.startsWith(prefix))
    }
    if (wordToChunkHead(sentence, index) != "_") {
      assigned(index).map { pred =>
        val predIndex = pred._1.left
        val distance = (math.min(predIndex, index) to math.max(predIndex, index)) map {
          case i if anyPrefix(sentence.words(i).ctag) => 1
          case _ => 0
        } sum

        if (distance > 5)
          s"$name-far-away"
        else
          s"$name-$distance"
      }.getOrElse("_")
    } else { "_" }
  }

  def matchingCaseNearby(sentence: Sentence, index: Int) = {
    def scanToLeft(distance: Int, gold: String) : Option[String] = {
      var result : Option[String] = None
      for (i <- (index - 1) to (index - distance).max(0) by -1) {
        val ctag = sentence.words(i).ctag.split(":")
        val pos = ctag(0)
        val case_ = { if (pos == "prep") {
            Some(ctag(1))
          } else if (pos == "subst") {
            Some(ctag(2))
          } else None
        }
        result = result.orElse { case_.filter(_ == gold) }
      }
      result
    }
    val myCtag = sentence.words(index).ctag.split(":")
    val matches = {
      if (myCtag(0) == "subst")
        scanToLeft(5, myCtag(2))
      else if (myCtag(0) == "prep")
        scanToLeft(5, myCtag(1))
      else
        None
    }
    matches.getOrElse("_")
  }

  def ppAttchment(cases: collection.Map[PPKey, PPValue], minProb: Double)(sentence: Sentence, index: Int) : String = {
    def prob(ppValue: PPValue) = ppValue.decision match {
      case 1 => ppValue.verb
      case 2 => ppValue.noun
    }
    val res : Option[String] = PPAttachmentProbs.extractForWord(sentence, index) match {
      case Some((word, index, nounHead, verbHead)) =>
        verbHead.flatMap { verb =>
          nounHead.flatMap { noun =>
            val key = PPKey(verb.base, noun.base, word.base, word.ctag.split(":")(1))
            cases.get(key).flatMap {
              case ppValue if (prob(ppValue) >= minProb) => Some(Seq("", "verb", "noun")(ppValue.decision))
              case _ => None
            }
          }
        }
      case None => None
    }
    res.getOrElse("_")
  }

  def ppAttachmentNoun(cases: collection.Map[PPKey, PPValue], minProb: Double)(sentence: Sentence, index: Int) : String = {
    def prob(ppValue: PPValue) = ppValue.decision match {
      case 1 => ppValue.verb
      case 2 => ppValue.noun
    }
    val res : Option[String] = PPAttachmentProbs.extractForWord(sentence, index) match {
      case Some((word, index, nounHead, verbHead)) =>
        verbHead.flatMap { verb =>
          nounHead.flatMap { noun =>
            val key = PPKey(verb.base, noun.base, word.base, word.ctag.split(":")(1))
            cases.get(key).flatMap {
              case ppValue if ppValue.decision == 2 && (prob(ppValue) >= minProb) => Some(Seq("", "verb", "noun")(ppValue.decision))
              case _ => None
            }
          }
        }
      case None => None
    }
    res.getOrElse("_")
  }

  def extractArgumentTypeLabels(sentence: Sentence): Seq[String] = {
    // TODO use extractArgumentWithPredicate
    val arguments = sentence.argumentStructures.valuesIterator.flatMap(identity _)

    val indexToLabel = arguments.flatMap {
      case argument if argument.syntacticHead.isDefined => Some(argument.syntacticHead.get -> argument.argumentType)
      case _ => None
    }.toMap

    for (index <- 0 until sentence.words.size) yield {
      // TODO: replace prepnp(xxx,case) with prepnp(_,case) (as xxx can be read from preposition base and
      // in case of rare preposition trying to recognize this causes errors (unnecessary)
      // TODO: think what to do with adjuncts (recognize their types... or maybe recognize them as adjuncts)
      // this requires some tests on what results can be achived as at some point I need to know whether it is
      // an argument or an adjunct (a binary classifier adjunct vs argument is an option)
      indexToLabel.getOrElse(Index(index), "_")
    }
  }

  def extractBothArgumentTypesAndAdjuncts(sentence: Sentence): Seq[String] = {
    val arguments = sentence.argumentStructures.valuesIterator.flatMap(identity _)
    val adjuncts = sentence.freePhrases.valuesIterator.flatMap(identity _)

    // TODO: refactor (copy-paste from above)
    val indexToLabel = (adjuncts ++ arguments).flatMap {
      case argument if argument.syntacticHead.isDefined => Some(argument.syntacticHead.get -> argument.argumentType)
      case _ => None
    }.toMap

    for (index <- 0 until sentence.words.size) yield {
      // TODO: replace prepnp(xxx,case) with prepnp(_,case) (as xxx can be read from preposition base and
      // in case of rare preposition trying to recognize this causes errors (unnecessary)
      // TODO: think what to do with adjuncts (recognize their types... or maybe recognize them as adjuncts)
      // this requires some tests on what results can be achived as at some point I need to know whether it is
      // an argument or an adjunct (a binary classifier adjunct vs argument is an option)
      sanitizeFeature(indexToLabel.getOrElse(Index(index), "_"))
    }
  }

  def extractNoArguments(sentence: Sentence): Seq[String] = {
    sentence.words.map(_ => "_")
  }

  def extractBothArgumentWithPredicate(sentence: Sentence) : Seq[Option[(Span, Argument)]] = {
    def merge[K,V](m1: collection.Map[K, Seq[V]], m2: collection.Map[K, Seq[V]]) = {
      (m1.keySet ++ m2.keySet).map(k => (k, m1.getOrElse(k, Seq()) ++ m2.getOrElse(k, Seq())))
    }
    val arguments = merge(sentence.argumentStructures, sentence.freePhrases)

    val indexToArgument: Map[Index, (Span, Argument)] = arguments.flatMap {
      case (predicateSpan, argumentList) =>
        argumentList.flatMap {
          case argument if argument.syntacticHead.isDefined =>
            Some(argument.syntacticHead.get -> (predicateSpan, argument))
          case _ => None
        }
    }.toMap

    for (index <- 0 until sentence.words.size) yield {
      indexToArgument.get(Index(index))
    }
  }

 class SkladnicaConllWriter(val featureExtractors: Iterable[FeatureExtractor[(Sentence, Int)]], labelsExtractor: (Sentence => Seq[String])) extends Serializable{
    val writer = new ConllWriter[((Sentence, Int), String)](featureExtractors.map(chooseFirstArgument), chooseSecondArgument)

    def extract(sentence: Sentence): Array[String] = {
      val labels = labelsExtractor(sentence)
      val input = (0 until sentence.words.size) map (i => (sentence, i)) zip labels
      input.map(writer.extract).toArray
    }

    private def chooseFirstArgument(f: FeatureExtractor[(Sentence, Int)])(argument: ((Sentence, Int), String)) = f(argument._1)

    private def chooseSecondArgument(argument: ((Sentence, Int), String)) = argument._2
  }

}