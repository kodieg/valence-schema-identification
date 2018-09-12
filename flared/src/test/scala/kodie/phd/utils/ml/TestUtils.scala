package kodie.phd.utils.ml

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by kodie on 9/21/14.
 */
class TestUtils extends FlatSpec with Matchers {
  "folds" should "generate appropriate test & train folds" in {
    val parts = DatasetUtils.folds(3, 10)

    parts.foreach {
      case (train, test) =>
        assert(test.size >= 3 && test.size <= 4)
        assert((train ++ test).toSet.size == 10)
    }

    assert(parts.flatMap(_._2).toSet.size == 10)
  }
}
