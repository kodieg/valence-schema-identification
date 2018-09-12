package kodie.phd.formats


import java.io._
import java.nio.charset.Charset

import breeze.linalg.{normalize, DenseVector}

/**
 * Created by kodie on 12/11/15.
 */
object Word2Vec {
  case class FileParameters(vocabSize: Int, vectorSize: Int)
  def readFileParameters(in: InputStream): FileParameters = {
    val params = readString(in, '\n'.toByte).split(" ").filterNot(_.trim.isEmpty).map(_.toInt)
    require(params.size == 2, "Invalid vectors file -- expected vocab size and vector size")
    FileParameters(params(0), params(1))
  }

  def readString(in: InputStream, until: Byte) : String = {
    val bytes = Iterator.continually(in.read.toByte).takeWhile(_ != until).toArray
    new String(bytes, Charset.forName("UTF-8"))
  }

  def readVector(in: InputStream, vectorSize: Int): DenseVector[Double] = {
    normalize(DenseVector(
      Array.fill(vectorSize) {
        readFloat(in).toDouble
      }
    ))
  }

  def readFloat(in: InputStream): Float = {
    val ch1 = in.read
    val ch2 = in.read
    val ch3 = in.read
    val ch4 = in.read
    if ((ch1 | ch2 | ch3 | ch4) < 0) throw new EOFException
    java.lang.Float.intBitsToFloat(((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0)))
  }

  def readBinary(in: InputStream): Map[String, DenseVector[Double]] = {
    val input = new BufferedInputStream(in)
    val parameters = readFileParameters(input)

    val pairs = for (i <- 0 until parameters.vocabSize) yield {
      val vocab = readString(input, ' '.toByte)
      val vec = readVector(input, parameters.vectorSize)
      //println(vocab, vec)
      //in.read() // why need to be commented?!

      //require(.toByte == '\r'.toByte, "Expected new line after the vector")
      //if (i > 10) ???

      vocab.trim() -> vec

    }
    pairs.toMap
  }
}

//object Main extends App {
//  val in = new FileInputStream("data/300m.bin")
//  val start = System.currentTimeMillis()
//
//  val vecs = Word2Vec.readBinary(in)
//
//  println(vecs.keys.take(10))
//  val man = vecs.get("mężczyzna")
//  val woman = vecs.get("kobieta")
//  val king = vecs.get("król")
//  val queen = vecs.get("królowa")
//
//  println(man, woman, king, queen)
//
//  for (m <- man; w <- woman; k <- king; q <- queen) yield {
//    val maybeQ = normalize(k + (w - m))
//    println("Queen:", q)
//    println("Near queen:", maybeQ)
//    println("distance: ", q dot maybeQ)
//  }
//
//  //println(x.toStream.take(10).mkString("\n"))
//  println(System.currentTimeMillis() - start)
//}