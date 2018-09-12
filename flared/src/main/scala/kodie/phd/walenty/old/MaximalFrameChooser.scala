package kodie.phd.walenty.old

/**
 * Created by kodie on 3/22/15.
 */
// This one is deprecated!
class MaximalFrameChooser(walenty: NewWalenty) extends Serializable {
  def findMaximalFrame(predicateBase: String, aspect: String, skladnicaArguments: Seq[Option[String]]) : String = {
    val originalPredicateFrame = findMaximalFrameForPredicate(predicateBase, aspect, skladnicaArguments)

    if (skladnicaArguments.exists(_.getOrElse("_") == "sie")) {
      if (!hasReflArgument(originalPredicateFrame)) {
        val argsWithoutSie = skladnicaArguments.map(_.filter(_ != "sie"))
        val predicateWithSie = predicateBase + " się"
        // I could check that such frame  exists, and if not fallback to originalPredicateFrame
        val sieFrame = findMaximalFrameForPredicate(predicateWithSie, aspect, argsWithoutSie)
        makeString("się", sieFrame._3, sieFrame._1)
      } else {
        makeString("", originalPredicateFrame._3, originalPredicateFrame._1)
      }
    } else {
      makeString("", originalPredicateFrame._3, originalPredicateFrame._1)
    }
  }

  def findMaximalFrameForPredicate(predicateBase: String, aspect: String, skladnicaArguments: Seq[Option[String]]) = {
    walenty.findMaximalFrame(predicateBase, aspect, skladnicaArguments)
  }

  def hasReflArgument(answ: (Option[IndexedSeq[WalentyArgument]], Seq[Option[WalentyArgument]], String)) = {
    answ._1.map(_.exists(_.realizations.contains("relf"))).getOrElse(false)
  }

  def makeString(sie: String, aspect: String, maybeFrame: Option[IndexedSeq[WalentyArgument]]) = {

    val maybePredicted = maybeFrame.map {
      frame =>
        s"$sie:_:$aspect:" + frame.map(_.toString.trim()).mkString("+")
    }
    val predicted = maybePredicted.getOrElse("none")
    predicted
  }
}
