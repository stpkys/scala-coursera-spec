package observatory

import java.io.File
import java.nio.file.Paths

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  import Visualization._

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    Location(
      lat = Math.atan(Math.sinh(Math.PI - y / Math.pow(2, zoom) * 2 * Math.PI)) * 180 / Math.PI,
      lon = x / Math.pow(2, zoom) * 360 - 180
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val (locs, temp) = prepareData(temperatures)
    val picture = new Array[Pixel](256*256)
    var i = 0
    while (i < 256 * 256) {
      val lx = i % 256
      val ly = i / 256
      val loc = tileLocation(8 + zoom, x * (128 << zoom) + lx, y * (128 << zoom) + ly)
      picture(i) = interpolateColor(colors, predictTemperature(locs, temp, loc)).toPixel(127)
      if (i % 10000 == 0) println(i)
      i += 1
    }
    Image(256, 256, picture)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val maxZoom = 3
    for {
      (year, data) <- yearlyData
      zoom <- 0 to maxZoom
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } {
      generateImage(year, zoom, x, y, data)
    }
  }

  def storeImage(year: Int, zoom: Int, x: Int, y: Int, data: Array[(Location, Double)]): Unit = {
    println(s"generating for $zoom on ($x-$y)")
    val image = tile(data, scale, zoom, x, y)
    val baseFolder = s"target/temperatures/$year/$zoom"
    new File(baseFolder).mkdirs()
    val outputFile = s"$baseFolder/$x-$y.png"
    image.output(Paths.get(outputFile))
  }

}
