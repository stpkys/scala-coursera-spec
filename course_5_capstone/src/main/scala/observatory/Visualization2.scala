package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.{interpolateColor, predictTemperature, prepareData}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  import observatory.Visualization.RichColor

  val scale: List[(Double, Color)] = ColorScales.scale2.sortBy(_._1)

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    visualizeGridSorted(grid, colors.toList.sortBy(_._1), zoom, x, y)
  }

  def visualizeGridSorted(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int
                   ): Image = {
    val picture = new Array[Pixel](256*256)
    (0 until 256 * 256).par.foreach(i => {
      val lx = i % 256
      val ly = i / 256
      val loc = tileLocation(8 + zoom, x * 256 + lx, y * 256 + ly)
      val lat = math.ceil(loc.lat).toInt
      val lon = math.floor(loc.lon).toInt
      val temp = bilinearInterpolation(
        loc.lon - lon,
        lat - loc.lat,
        grid(lat, lon), grid(lat - 1, lon), grid(lat, lon + 1), grid(lat - 1, lon + 1))
      picture(i) = interpolateColor(colors, temp).toPixel(127)
    })
    Image(256, 256, picture)
  }

  def testGrid(lat: Int, lon: Int): Double = {
    14 * (180 + lon) / 360.0 - 7
  }

  def main(args: Array[String]) = {
    Main.saveTmpImage(visualizeGrid(testGrid, scale, 0, 0, 0), "target/test_grid.png")
  }
}
