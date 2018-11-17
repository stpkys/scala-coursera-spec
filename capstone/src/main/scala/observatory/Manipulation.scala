package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

//  class Grid(grid: ((Int, Int) => Double)) extends ((Int, Int) => Double) {
//    val size: Int = 360 * 180
//    val cache: Array[Double] = new Array[Double](360 * 180)
//
//    (0 until size).foreach({ i =>
//      val lat = Converter.latFromIndex(i)
//      val lon = Converter.lonFromIndex(i)
//      cache(i) = grid(lat, lon)
//    })
//
//    def apply(lat: Int, lon: Int): Double = {
//      cache(Converter.toIndex(lat, lon))
//    }
//  }


  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): ((Int, Int) => Double) = {
    val (locations, temps) = Visualization.prepareData(temperatures)
//
//    def innerData(lat: Int, lon: Int): Double = {
//      Visualization.predictTemperature(locations, temps, Location(lat, lon))
//    }
//
//    new Grid(innerData)

    (lat: Int, lon: Int) => {
      Visualization.predictTemperature(locations, temps, Location(lat, lon))
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val grids: Iterable[(Int, Int) => Double] = temperaturess.map(makeGrid)
    (lat: Int, lon: Int) => {
      val r = grids.foldLeft((0.0, 0))((acc, f) => (acc._1 + f(lat, lon), acc._2 + 1))
      r._1 / r._2
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    def grid = makeGrid(temperatures)

    (lat: Int, lon: Int) => {
      grid(lat, lon) - normals(lat, lon)
    }
  }

}

