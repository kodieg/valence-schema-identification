package kodie.phd.tools.wordnet

import java.io._

import scala.collection.mutable
import scala.io.{Codec, Source}
import scala.xml.pull.{EvText, EvElemEnd, EvElemStart, XMLEventReader}


case class Synset(id: Int, lexUnits: IndexedSeq[Int])
case class LexicalUnit(id: Int, name: String, variant: Int, pos: String, domain: String)

case class PLWordnet(val units: collection.Map[Int, LexicalUnit],
                     val synsets: collection.Map[Int, Synset],
                     val hypernyms: collection.Map[Int, Array[Int]]) {
  val dictionary = units.toStream.map(u => u._2.name -> u._1).groupBy(_._1).mapValues(_.map(_._2).toArray).map(identity)
  val luToSynsets = synsets.toStream.flatMap(u => u._2.lexUnits.map(lu => lu -> u._1)).groupBy(_._1).mapValues(_.map(_._2).toArray).map(identity)

  def lexicalUnitName(id: Int) = {
    units.get(id).map(lu => s"${lu.name} ${lu.variant}").getOrElse("unknown lu!")
  }

  def synsetName(id: Int) = {
    synsets.get(id).map(synset => synset.lexUnits.map(lexicalUnitName).mkString(", ")).getOrElse("unknown synset!")
  }

  def topSynsets(luName: String) : collection.Set[Int] = {
    dictionary.get(luName).map { units =>
        val synsets = units.iterator.flatMap(unit => luToSynsets.get(unit)).flatten.toSet
        synsets.flatMap(topSynsetsForSynset)
    }.getOrElse(Set())
  }

  def topSynsetsForSynset(synset: Int) : collection.Set[Int] = {
    val topSynsets = mutable.Set[Int]()
    val visited = mutable.Set[Int]()

    def go(current: Int) {
      visited += current
      val parents = hypernyms.get(current)
      if (parents.isEmpty) {
        topSynsets += current
      } else {
        for (hypers <- parents;
             nextSynset <- hypers if !visited.contains(nextSynset)) {
          go(nextSynset)
        }
      }
    }

    go(synset)
    topSynsets
  }
}

object PLWordnet {
  val HYPERNYM = 11

  def cached() : PLWordnet = {
    if (new File("plwordnet.cache").exists()) {
      val ois = new ObjectInputStream(new FileInputStream("plwordnet.cache"))
      val plWordnet = ois.readObject().asInstanceOf[PLWordnet]
      ois.close()
      plWordnet
    } else {
      val plWordnet = load()
      val out = new ObjectOutputStream(new FileOutputStream("plwordnet.cache"))
      out.writeObject(plWordnet)
      out.close()
      plWordnet
    }
  }

  def load() : PLWordnet = load(getClass.getResourceAsStream("/plwordnet_2_0.xml"))

  def load(input: InputStream) : PLWordnet = {
    val lu = mutable.Map[Int,LexicalUnit]()
    val synsets = mutable.Map[Int,Synset]()

    var currentSynset: Option[Int] = None
    val currentSynsetUnits = mutable.ArrayBuffer[Int]()

    val hypernymsPairs = mutable.ArrayBuffer[(Int, Int)]()  // child -> parent

    var insideUnitId = false

    val xmlEvents = new XMLEventReader(Source.fromInputStream(input)(Codec.UTF8))


    for (event <- xmlEvents) {
      event match {
        case EvElemStart(_, "lexical-unit", attrs, _) =>
          val id = attrs.get("id").flatMap(_.headOption.map(_.text.toInt)).getOrElse(-1)
          val name = attrs.get("name").flatMap(_.headOption.map(_.text)).getOrElse("")
          val variant = attrs.get("variant").flatMap(_.headOption.map(_.text.toInt)).getOrElse(-1)
          val pos = attrs.get("pos").flatMap(_.headOption.map(_.text)).getOrElse("")
          val domain = attrs.get("domain").flatMap(_.headOption.map(_.text)).getOrElse("")
          lu += (id -> LexicalUnit(id, name, variant, pos, domain))
        case EvElemStart(_, "synset", attrs, _)  =>
          currentSynset = Some(attrs.get("id").flatMap(_.headOption.map(_.text.toInt)).getOrElse(-1))
        case EvElemStart(_, "unit-id", _, _) => insideUnitId = true
        case EvText(text) if insideUnitId =>
          currentSynsetUnits += text.toInt
        case EvElemEnd(_, "unit-id") =>
          insideUnitId = false
        case EvElemEnd(_, "synset") =>
          currentSynset.foreach { synsetId =>
            synsets += synsetId -> Synset(synsetId, currentSynsetUnits.toArray)
          }
          currentSynsetUnits.clear()
        case EvElemStart(_, "synsetrelations", attrs, _) if (attrs.get("relation").flatMap(_.headOption).map(_.text.toInt) == Some(HYPERNYM)) =>
          val parent = attrs.get("parent").flatMap(_.headOption.map(_.text.toInt)).getOrElse(-1)
          val child = attrs.get("child").flatMap(_.headOption.map(_.text.toInt)).getOrElse(-1)
          hypernymsPairs += (child -> parent)
        case _ =>  // ignore
      }

    }

    val hypernyms = hypernymsPairs.groupBy(_._1).mapValues(_.map(_._2).toArray).map(identity)

    PLWordnet(lu, synsets, hypernyms)
  }
}
