package kodie.phd.tools.crf

import java.io.PrintWriter
import scala.util.Random

class CRFModel {}
/**
 * Created by kodie on 9/17/14.
 */
class CRF(val modelPath: String, driver: CRFDriver) extends Serializable {
  def predict(dataset: Traversable[Array[String]], testPath : String = "dataset.test", useV2: Boolean = false) = {
    if (dataset.nonEmpty) {
      CRF.storeDataset(testPath, dataset)
      val numFeats = dataset.head.size

      driver.test(modelPath, testPath, useV2)
    } else {
      Stream.empty
    }
  }
}


object CRF {
  def train(features: String, dataset: Traversable[Array[String]], driver: CRFDriver = LocalCRFDriver) = {
    val trainPath = "dataset.train"
    val featuresPath = "features"
    val modelPath = "model"

    storeFeatures(featuresPath, features)
    storeDataset(trainPath, dataset)
    // run crf_learn
    driver.learn(featuresPath, trainPath, modelPath)
    // todo: configure paths and learning options
    new CRF(modelPath, driver)
  }

  def storeDataset(filePath: String, dataset: Traversable[Array[String]]) {
    val datasetFile = new PrintWriter(filePath) // TODO: configure paths and try finally _.close
    dataset.foreach { sentence =>
      sentence.foreach(datasetFile.println)
      datasetFile.println()
    }
    datasetFile.close
  }

  def storeFeatures(filePath: String, features: String) {
    val featuresFile = new PrintWriter(filePath)
    featuresFile.println(features)
    featuresFile.close
  }
}