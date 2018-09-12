package kodie.phd.tools.crf

import sys.process._

abstract class CRFDriver extends Serializable {
  def learn(featuresPath: String, trainPath: String, modelPath: String)
  def test(modelPath: String, testPath: String, useV2: Boolean = false): Stream[String]
}

class CustomizedLocalCRFDriver(algo: String, cost: Double, feats: Int, maxIter: Int = 1000) extends CRFDriver {
  override def learn(featuresPath: String, trainPath: String, modelPath: String) {
    Seq("nice", "-n", "19", "crf_learn", "-f", feats.toString, "-c", cost.toString, "-a", algo, "-m", maxIter.toString, featuresPath, trainPath, modelPath).!
  }

  override def test(modelPath: String, testPath: String, useV2: Boolean = false): Stream[String] = {
    val v2Seq = if (useV2) Seq("-v2") else Seq()
    (Seq("crf_test") ++ v2Seq ++ Seq("-m", modelPath, testPath)).lines
  }
}

object LocalCRFDriver extends CRFDriver {
  override def learn(featuresPath: String, trainPath: String, modelPath: String) {
    // TODO: remove -m 1!
    Seq("nice", "-n", "19", "crf_learn", "-f", "10", "-m", "1000", featuresPath, trainPath, modelPath).!
  }

  override def test(modelPath: String, testPath: String, useV2: Boolean = false): Stream[String] = {
    val v2Seq = if (useV2) Seq("-v2") else Seq()
    (Seq("crf_test") ++ v2Seq ++ Seq("-m", modelPath, testPath)).lines
  }
}

class RemoteCRFDriver(sshConnection: String) extends CRFDriver {
  // TODO: refactor, make configurable, make common with Local...
  override def learn(featuresPath: String, trainPath: String, modelPath: String) {
    Map("features" -> featuresPath, "train" -> trainPath).foreach {
      case (fileName, localPath) => Seq("rsync", "-avz", localPath, s"${sshConnection}:$fileName").!
    }
    Seq("ssh", sshConnection, "nice", "-n", "19", "CRF++-0.58/crf_learn", "-f", "200", "-m", "10000", "features", "train", "model").!
    Seq("rsync", "-avz", s"$sshConnection:model", modelPath).!
  }

  override def test(modelPath: String, testPath: String, useV2: Boolean = false): Stream[String] = {
    Map("model" -> modelPath, "test" -> testPath).foreach {
      case (fileName, localPath) => Seq("rsync", "-avz", localPath, s"$sshConnection:$fileName").!
    }
    val v2Seq = if (useV2) Seq("-v2") else Seq()
    (Seq("ssh", sshConnection, "nice", "-n", "19", "CRF++-0.58/crf_test") ++ v2Seq ++ Seq("-m", "model", "test")).lines
  }

}
