package kodie.phd.tools.xgboost

import org.apache.spark.mllib.linalg.{SparseVector, DenseVector}
import org.apache.spark.mllib.regression.LabeledPoint

/**
 * Created by kodie on 2/24/16.
 */
object LibSVMFormat {
  def pointToString(p: LabeledPoint) = {
    val feats = p.features match {
      case dv: DenseVector => dv.toArray.zipWithIndex.filter(_._1 != 0).map(x => s"${x._2}:${x._1}")
      case sv: SparseVector => sv.values.zip(sv.indices).filter(_._1 != 0).map(x => s"${x._2}:${x._1}")
    }
    s"${p.label.toInt} ${feats.mkString(" ")}"
  }
}
