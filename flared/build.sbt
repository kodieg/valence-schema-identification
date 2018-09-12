//import AssemblyKeys._

name := "Flared"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.0" % "test"

libraryDependencies += ("org.apache.spark" %% "spark-core" % "1.6.0")

libraryDependencies += ("org.apache.spark" %% "spark-mllib" % "1.6.0")

libraryDependencies += ("com.databricks" %% "spark-csv" % "1.5.0")

libraryDependencies += ("org.apache.commons" % "commons-lang3" % "3.4")

// libraryDependencies += "org.apache.hadoop" % "hadoop-client" % "2.4.0"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1.1"

libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.12"
libraryDependencies += "nz.ac.waikato.cms.weka" % "LibLINEAR" % "1.9.7"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"




resolvers += "Akka Repository" at "http://repo.akka.io/releases/"

//assemblySettings

//mainClass in assembly := Some("experimental.ExtractStructureFromCCL")

fork in run := true

def sysPropOrDefault(propName:String,default:String):String = Option(System.getProperty(propName)).getOrElse(default)

javaOptions in run ++= Seq(
        "-Xmx" + sysPropOrDefault("runMain.memory", "75G")
        )


//test in assembly := {}

//net.virtualvoid.sbt.graph.Plugin.graphSettings
/*

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
{
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList("javax", "transaction", xs @ _*)     => MergeStrategy.first
  case PathList("javax", "mail", xs @ _*)     => MergeStrategy.first
  case PathList("javax", "activation", xs @ _*)     => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case PathList("META-INF", "ECLIPSEF.RSA") => MergeStrategy.first
  case PathList("META-INF", "mailcap") => MergeStrategy.first
  case PathList("META-INF", "mimetypes.default") => MergeStrategy.first
  case "application.conf" => MergeStrategy.concat
  case "unwanted.txt"     => MergeStrategy.discard
  case x => old(x)
  }
}

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in oneJar := Some("experimental.ExtractStructureFromCCL")
*/
connectInput in run := true
