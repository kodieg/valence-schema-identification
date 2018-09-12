package kodie.phd.utils

import scala.util.Random

/**
 * Created by kodie on 9/21/14.
 */
package object ml {
  object DatasetUtils {
    def folds(k: Int, n: Int): Seq[(Traversable[Int], Traversable[Int])] = {
      val indexes = Random.shuffle((0 until n) toIndexedSeq)
      val folded = indexes.zipWithIndex.groupBy(_._2 % k).mapValues(_.map(_._1))

      val result = for ((foldIndex, testPart) <- folded) yield {
        val trainPart = (0 until k).toStream filter (_ != foldIndex) flatMap (folded.apply _)
        (trainPart.toTraversable, testPart.toTraversable)
      }
      result.toSeq
    }

    def folds[T](k: Int, dataset: IndexedSeq[T]): Seq[(Stream[T], Stream[T])] = {
      val parts = folds(k, dataset.size)

      parts.map {
        case (train, test) => (train.toStream.map(dataset.apply _), test.toStream.map(dataset.apply _))
      }
    }
  }
}
