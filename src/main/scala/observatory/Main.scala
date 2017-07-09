package observatory

import java.io.File
import java.lang.Thread

import com.sksamuel.scrimage.Image
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source


object Main extends App {

  def measure[A](comment: String)(f: => A): A = {
    val start = System.nanoTime()
    val result = f
    val stop = Math.round((System.nanoTime() - start) / (1000.0 * 1000.0))
    println(s"$comment: $stop ms")
    result
  }

  lazy val sparkConf = new SparkConf().setMaster("local").setAppName("observatory")
  lazy val sc = {
    import org.apache.log4j.{Level, Logger}
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    val sc = new SparkContext(sparkConf)
    sc
  }

  import Extraction._

  lazy val oldDataSet = measure("join datasets") {
    val tmp = locateTemperatures(2010, defaultStationFile, defaultTemperatureFile)
    println("temperatures with locations " + tmp.take(10))
    val d = locationYearlyAverageRecords(tmp)
    println(s"average ${d.size} ${d.take(10)}")
    d
  }

  import Visualization._

  lazy val d: Array[(Location, Double)] = measure("join dataSets") {
    val t = locateTemperaturesRDD(2015, defaultStationFile, defaultTemperatureFile).collect()
//    println(s"average in rdd ${t.length}")
    t
  }

  def saveTmpImage(img: Image, output: String = "target/output.png"): Unit = {
    img.output(new java.io.File(output))
  }

//  println(d.length)
  val image = measure("single year visualisation") {
    visualize(d, Visualization.scale)
  }
  saveTmpImage(image, "target/single_year.png")

//  val image = measure("genereate tile") {
//    import Interaction._
//    tile(d, Visualization.scale, 0, 0, 0)
//  }
//  saveTmpImage(image, "target/tile.png")

//
//  Interaction.generateTiles[Array[(Location, Double)]](List((2015, d)), Interaction.storeImage)

//  val stationsImage = measure("vsualize stations") {
//    visualizeStations(d, Visualization.scale)
//  }
//
//  val stationsImageOld = measure("visualize stations again") {
//    visualizeStations(oldDataSet, Visualization.scale)
//  }

//  measure("Saving Image") {
//    saveTmpImage(stationsImage, "target/stations_1.png")
//    saveTmpImage(stationsImageOld, "target/stations_old_1.png")
//  }
}