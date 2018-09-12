package kodie.phd.walenty

import java.io._
import java.util

import breeze.linalg.DenseVector
import phd.sr.data.{WordsIndex, PredicateGroupDataset}
import weka.classifiers.{AbstractClassifier, Classifier}
import weka.classifiers.functions.LibLINEAR
import weka.core.{Attribute, DenseInstance, Instance, Instances}

import scala.collection.mutable
import scala.util.Random
import scala.util.control.NonFatal


trait VectorBasedSR {
  def computeSimilarity(predicate: Long, groupType: Long, d: DenseVector[Double]): Double
  def maybeComputeSimilarity(predicate: Long, groupType: Long, d: DenseVector[Double]): Option[Double]
}

object SchemaVectorsProviders {
  val FACTOR = 10000000L
  def calculate(path: String, dataset: PredicateGroupDataset, index: WordsIndex, word2Vec: Map[String, DenseVector[Double]]): collection.Map[Long,  DenseVector[Double]] = {
    val cachePath = path + s"/schvecs.bin"
    if (new File(cachePath).exists) {
      try {
        return loadCache(cachePath)
      } catch {
        case NonFatal(e) => println(s"Exception: $e")
      }
    }

    val result = calculate_(dataset, index, word2Vec)
    saveCache(cachePath, result)
    result
  }

  def loadCache(path: String) = {
    val ios = new ObjectInputStream(new FileInputStream(path))
    val counts = ios.readObject().asInstanceOf[collection.Map[Long,  DenseVector[Double]]]
    ios.close()
    counts
  }

  def saveCache(path: String, counts: collection.Map[Long,  DenseVector[Double]]) {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(counts)
    oos.close()
  }

  // TODO: to do automatic clustering it needs to be done differently somehow....
  def calculate_(dataset: PredicateGroupDataset, index: WordsIndex, word2Vec: Map[String, DenseVector[Double]]) = {

    val zeros = DenseVector.zeros[Double](300)
    val res = mutable.Map[Long, DenseVector[Double]]().withDefaultValue(zeros)
    val cnt = mutable.Map[Long, Int]().withDefaultValue(0)

    dataset.foreach { gi =>
      val word = index.word(gi.groupSemanticHeadOrth)
      val predicate = gi.predicateLemma.toLong
      val group = gi.groupType.toLong
      val key = FACTOR * predicate + group

      res(key) = res(key) + word2Vec.getOrElse(word, zeros)
      cnt(key) = cnt(key) + 1
    }

    for (p <- res.keys) {
      res(p) = res(p) / cnt(p).toDouble
    }

    res
  }
}

class CosineSimilarity(mapping: collection.Map[Long, DenseVector[Double]]) extends VectorBasedSR{
  def computeSimilarity(predicate: Long, group: Long, d: DenseVector[Double]): Double = {
    val key = SchemaVectorsProviders.FACTOR * predicate + group
    mapping.get(key) match {
      case Some(u) =>
        val vv = u dot d
        val z = math.sqrt((u dot u) * (d dot d))
        if (z != 0.0) vv / z
        else 0.0
      case _ => 0
    }
  }

  def maybeComputeSimilarity(predicate: Long, group: Long, d: DenseVector[Double]): Option[Double] = {
    val key = SchemaVectorsProviders.FACTOR * predicate + group
    mapping.get(key).map { _ =>
      computeSimilarity(predicate, group, d)
    }
  }
}

object SchemaLogRegVectorsProviders {
  val FACTOR = 10000000L

  val MIN_EXAMPLES = 301 // TODO: can this be smaller or this would lead to infinite number of solutions?!

  def makeInstance(label: Int, v: DenseVector[Double]) = {
    val inst = new DenseInstance(v.length + 2)
    var i = 1
    inst.setValue(0, 1)
    for (v <- v.toArray) {
      inst.setValue(i, v)
      i+=1
    }
    inst.setValue(i, label.toDouble)
    inst
  }
  def calculate(path: String, dataset: PredicateGroupDataset, index: WordsIndex, word2Vec: Map[String, DenseVector[Double]]): collection.Map[Long, AbstractClassifier] = {
    val cachePath = path + s"/logregsr2.bin"
    if (new File(cachePath).exists) {
      try {
        return loadCache(cachePath)
      } catch {
        case NonFatal(e) => println(s"Exception: $e")
      }
    }

    val result = calculate_(dataset, index, word2Vec)
    saveCache(cachePath, result)
    result
  }

  def loadCache(path: String) = {
    val ios = new ObjectInputStream(new FileInputStream(path))
    val counts = ios.readObject().asInstanceOf[collection.Map[Long, AbstractClassifier]]
    ios.close()
    counts
  }

  def saveCache(path: String, counts: collection.Map[Long, AbstractClassifier]) {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(counts)
    oos.close()
  }


  def calculate_(dataset: PredicateGroupDataset, index: WordsIndex, word2Vec: Map[String, DenseVector[Double]]) = {
    // TODO: to do support cacheing!!!!
    val zeros = DenseVector.zeros[Double](300)

    val word2Inst = word2Vec.map {
      case (k, v) => k -> makeInstance(1, v)
    }

    val numVecs = word2Vec.size
    val expRatio = 500000.0f / numVecs
    val r = new Random()
    val randoms = word2Vec.iterator.filter(_ => r.nextFloat() <= expRatio).map { v =>
      makeInstance(0, v._2)
    }.toArray /*Array.tabulate(500000) { _ =>
    // TODO: I could make sure that this random vector isn't too close to some example, but
    // that's computionally expensive)=
    makeInstance(0, DenseVector.rand[Double](LEN))
  }*/ // Other option is to normalize all vectors and draw from sphere (above solution is improper! it choses rand from [0,1]^n cube!


    println(s"Calculating log reg vecs --> sorting data")
    val data = dataset.map(d => (d.predicateLemma, d.groupType, d.groupSemanticHeadOrth)).toArray
    util.Arrays.sort(data, implicitly[Ordering[(Int, Int, Int)]])
    println("log reg vecs --> sorting done")

    val res = mutable.Map[Long, AbstractClassifier]()

    var lastPred = -1
    var lastGroup = -1
    val params = mutable.ArrayBuffer[Instance]()

    val cnt = data.size
    var i = 0

    (data.iterator ++ Iterator.single((-1000, -1000, -1000))).foreach {
      case (predicate, groupType, orth) =>
        if ((i % 100000) == 0) {
          println(s"LOGREG: $i/$cnt --> $predicate, $groupType (params: ${params.size}  --> ts: ${System.currentTimeMillis()} --> res: ${res.size } ")
        }
        i += 1
        if (predicate != lastPred || groupType != lastGroup) {
          if (lastPred != -1 && params.size >= MIN_EXAMPLES) {
            val classifier = train(params, randoms)
            val key = FACTOR * lastPred + lastGroup
            res(key) = classifier
          }
          params.clear()
          lastPred = predicate
          lastGroup = groupType
        }
        word2Inst.get(index.getWordOrElse(orth, "")).foreach { v =>
          params += v
        }

    }

    res
  }
  val LEN = 300



  def train(examples: mutable.ArrayBuffer[Instance], randoms: Array[DenseInstance]) = {
    // TODO: maybe work on regularization -- default is 1.0 for L2?

    println(s"${System.currentTimeMillis()} PREPARING TO TRAIN")
    val attInfo = new util.ArrayList[Attribute]()
    for (i<- 0 until (LEN+1)) {
      attInfo.add(i, new Attribute(s"v_$i"))
    }
    val vals = new util.ArrayList[String]()
    vals.add("0")
    vals.add("1")
    attInfo.add(LEN+1, new Attribute("theClass", vals))

    //println(s"${System.currentTimeMillis()} Adding examples")

    val instances = new Instances("ds", attInfo, examples.size * 2)
    instances.setClassIndex(attInfo.size - 1)
    (examples).foreach { instance =>
      instances.add(instance)
    }

    //println(s"${System.currentTimeMillis()} Adding randoms")

// limited by 500k anyway!
    randoms.view.take(examples.size * 10).foreach { ins =>
      instances.add(ins)
    }

    //val cls = new Logistic()
    val cls = new LibLINEAR
    cls.setOptions(Array("-S", "0", "-P"))
    //println(s"${System.currentTimeMillis()} TRAINING")
    cls.buildClassifier(instances)
    //println(s"${System.currentTimeMillis()} TRAINING DONE")

    //println(s"RESULT: ${cls.distributionForInstance(instances.get(0)).toList}")
    //println(s"RESULT: ${cls.distributionForInstance(randoms(0)).toList}")
    //println(s"RESULT: ${cls.distributionForInstance(makeInstance(1, DenseVector.rand[Double](LEN))).toList}")
    cls
  }
}


class LogRegressionSimilarity(mapping: collection.Map[Long, AbstractClassifier]) extends VectorBasedSR {
  def computeSimilarity(predicate: Long, group: Long, d: DenseVector[Double]): Double = {
    val key = SchemaVectorsProviders.FACTOR * predicate + group
    mapping.get(key) match {
      case Some(u) =>
        val score = u.synchronized { u.distributionForInstance(SchemaLogRegVectorsProviders.makeInstance(0, d))(1) * 2 - 1 }
        score

      case _ => 0
    }
  }

  def maybeComputeSimilarity(predicate: Long, group: Long, d: DenseVector[Double]): Option[Double] = this.synchronized {
    val key = SchemaVectorsProviders.FACTOR * predicate + group
    mapping.get(key).map { _ =>
      computeSimilarity(predicate, group, d)
    }
  }

}