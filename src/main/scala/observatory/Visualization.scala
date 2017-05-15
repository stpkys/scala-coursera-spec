package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val R = 6371.0

  val scale: List[(Double, Color)] = List[(Double, Color)](
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  ).sortBy(_._1)

  def distance(l1: Location, l2: Location): Double = {
    distance(locationToLocationRad(l1), locationToLocationRad(l2))
  }

  def distance(l1: LocationsRad, l2: LocationsRad): Double = {
    val delta = fastACos(l1.latSin * l2.latSin + l1.latCos * l2.latCos * cos(abs(l1.lon - l2.lon)))
    R * delta
  }

  def fastACos(a: Double): Double = {
    val negate = if (a < 0) 1.0 else 0.0
    val x = math.abs(a)
    val y = (((-0.0187293 * x + 0.0742610) * x - 0.2121144) * x + 1.5707288) * math.sqrt(1.0 - x)
    val z = y - 2 * negate * y
    negate * 3.14159265358979 + z
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val (locations, temps) = prepareData(temperatures)
    predictTemperature(locations, temps, location)
  }

  def locationToLocationRad(location: Location): LocationsRad = {
    val latRad = toRadians(location.lat)
    LocationsRad(latRad, toRadians(location.lon), sin(latRad), cos(latRad))
  }

  def predictTemperature(locations: Array[LocationsRad], temperatures: Array[Double], location: Location): Double = {
    val p = 3.0
    val k = temperatures.length
    val currentLocation = locationToLocationRad(location)

    @tailrec
    def loop(i: Int, a: Double, b: Double): Double = {
      if (i >= k) {
        a / b
      } else {
        val t = temperatures(i)
        val x = locations(i)
        val d = distance(currentLocation, x)
        if (d <= 1.0) {
          t
        } else {
          val w = 1 / Math.pow(d, p)
          loop(i + 1, a + w * t, b + w)
        }
      }
    }
    loop(0, 0.0, 0.0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def linear(a: (Double, Color), b: (Double, Color), v: Double): Color = {
      val x_0 = a._1
      val x_1 = b._1

      def i(y_0: Double, y_1: Double): Int = {
        val norm = (v - x_0) / (x_1 - x_0)
        Math.round(y_0 * (1 - norm) + y_1 * norm).toInt
      }

      Color(i(a._2.red, b._2.red), i(a._2.green, b._2.green), i(a._2.blue, b._2.blue))
    }

    def go(p: List[(Double, Color)]): Color = p match {
      case Nil => throw new IllegalArgumentException("empty list")
      case a :: Nil => a._2
      case a :: b :: tail =>
        if (value <= a._1) a._2
        else if (value < b._1) linear(a, b, value)
        else go(b :: tail)
    }

    go(points.toList)
  }

  implicit class RichColor(color: Color) {
    def toPixel: Pixel = toPixel()
    def toPixel(opacity: Int = 0xFF): Pixel = Pixel(color.red, color.green, color.blue, opacity)
  }

  def prepareData(temperatures: Iterable[(Location, Double)]): (Array[LocationsRad], Array[Double]) = {
    val unzipped = temperatures.unzip
    val locations = unzipped._1.toArray
      .map(l => locationToLocationRad(l))
    val temps = unzipped._2.toArray
    (locations, temps)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val size = 360 * 180
    val t = new Array[Pixel](size)
    val (locations, temps) = prepareData(temperatures)

    (0 until size).par.foreach(i => {
      val lx = i % 360 - 180
      val ly = 90 - (i / 360)
      t(i) = interpolateColor(colors, predictTemperature(locations, temps, Location(ly, lx))).toPixel
    })

    Image(360, 180, t)
  }

  def visualizeStations(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val t = new Array[Pixel](360 * 180)
    val temps = new Array[(Double, Int)](360*180)

    (0 until 360 * 180).foreach(i => t(i) = Pixel(0xFF, 0xFF, 0xFF, 0xFF))
    (0 until 360 * 180).foreach(i => temps(i) = (0.0, 0))

    temperatures.foreach {
      case (loc, temp) =>
        val lx = Math.min(Math.round(loc.lon).toInt + 180, 359)
        val ly = Math.min(90 - Math.round(loc.lat).toInt, 179)
        val idx = ly * 360 + lx
        temps(idx) = (temps(idx)._1 + temp, temps(idx)._2 + 1)
    }
    (0 until 360*180).foreach({ i => if (temps(i)._2 != 0) t(i) = interpolateColor(colors, temps(i)._1 / temps(i)._2).toPixel })
    Image(360, 180, t)
  }
}

