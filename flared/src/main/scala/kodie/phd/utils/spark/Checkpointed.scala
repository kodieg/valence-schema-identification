package kodie.phd.utils.spark

import org.apache.spark.rdd.RDD
import java.nio.file.{Paths, Files}
import org.apache.spark.SparkContext
import scala.reflect.ClassTag

/**
 * Created by kodie on 9/19/14.
 */
object Checkpointed {
  def apply[T: ClassTag](sc: SparkContext, path: String)(builder: =>RDD[T]): RDD[T] = {
    if (Files.exists(Paths.get(path))){
      sc.objectFile[T](path)
    } else {
      val result = builder
      result.saveAsObjectFile(path)
      result
    }
  }
}
