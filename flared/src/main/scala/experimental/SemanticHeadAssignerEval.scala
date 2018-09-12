package experimental

import kodie.phd.formats.NKJP1MJson
import kodie.phd.assigner.SemanticHeadAssigner
import scala.collection.mutable

// TODO: move out of experimental; refactor?
object SemanticHeadAssignerEval extends App{
  val sentGroupSeq = NKJP1MJson.load()

  var correct = 0
  var total = 0
  val byGroupCorrect = mutable.Map[String, Int]().withDefaultValue(0)
  val byGroupTotal = mutable.Map[String, Int]().withDefaultValue(0)

  for (sentGroup <- sentGroupSeq) {
    val argsLabelsWithSemHead = NKJP1MJson.argumentsToOptionSeq(sentGroup._1, sentGroup._2)
    val argsLabels = argsLabelsWithSemHead.map(_.map(_._1).getOrElse("_"))

    val semheads = SemanticHeadAssigner.assignToArgs(sentGroup._1.words, argsLabels)

    val sentScore = semheads.zip(argsLabelsWithSemHead.map(_.map(_._2))).map {
      case (Some(assigned), Some(gold)) if assigned == gold => 1
      case _ => 0
    }.sum

    semheads.zip(argsLabelsWithSemHead).foreach {
      case (Some(assigned), Some((argType, gold))) =>
        if (gold == assigned) byGroupCorrect(argType) += 1
        byGroupTotal(argType) += 1
      case (_, Some((argType, _))) => byGroupTotal(argType)
      case _ => ;
    }

    var npWrong = false
    val words = sentGroup._1.words
    val sentText = sentGroup._1.words.zip(argsLabelsWithSemHead).zip(semheads).map {
      case ((word, gold), assigned) =>
        val t = gold.map(_._1).getOrElse("_")
        val orth = word.orth + "/" + word.ctag.split(":")(0)
        val goldOrth = gold.map(x => "gold:"+words(x._2).orth + " ").getOrElse("")
        val assignedOrth = assigned.map(x => "assigned:"+words(x).orth + " ").getOrElse("")
        if (word.ctag.startsWith("subst") && gold.map(_._1).getOrElse("_").startsWith("np") && assigned.getOrElse(-1) != gold.map(_._2).getOrElse(-2)) {
          npWrong = true
        }

        val extraText = goldOrth + assignedOrth
        val extraTextWithMarks = if (extraText.isEmpty)  "" else s"($t/$extraText)"

        s"$orth$extraTextWithMarks"
    }.mkString(" ")

    val sentTotal = argsLabelsWithSemHead.filter(_.isDefined).size


    if (npWrong) {
      //  println(sentText)
      //throw new RuntimeException("")
    }

    correct += sentScore
    total += sentTotal
  }

  println("total accuracy: %.4f\n" format (correct.toFloat * 100 / total))
  byGroupTotal.keys.filter(key => byGroupTotal(key) > 10).toSeq.sortBy(key => -byGroupCorrect(key) * 100.0 / byGroupTotal(key)).foreach { key =>
    println("%s: %.4f (%d / %d)" format (key, byGroupCorrect(key) * 100.0 / byGroupTotal(key), byGroupCorrect(key), byGroupTotal(key)))
  }
}
