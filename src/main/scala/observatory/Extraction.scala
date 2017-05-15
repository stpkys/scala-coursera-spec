package observatory

import java.io.File
import java.time.LocalDate

import org.apache.spark.rdd.RDD


/**
  * 1st milestone: data extraction
  */
object Extraction {
  import Main._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations: RDD[(StationId, Iterable[Station])] =
      getStationsRRDOld(stationsFile).groupBy(_.id)
    println("stations size old " + stations.count())
    val temperatures: RDD[(StationId, Iterable[TemperatureRecord])] =
      getTemperatureRDD(temperaturesFile).groupBy(_.id)

    stations
      .join(temperatures)
      .flatMapValues({ case (station, temps) =>
        temps.map(record => (LocalDate.of(year, record.month, record.day), station.head.location, record.temp))
      })
      .values
      .collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val rdd = sc.parallelize(records.toSeq)
    val result = rdd
      .groupBy({case (_, location, _) => location})
      .mapValues(t =>
        t.map(k => (k._3, 1))
        .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
      )
    result.collect().map({case (loc, p) => (loc, p._1 / p._2)})
  }

  val defaultStationFile = "/stations.csv"
  val defaultTemperatureFile = "/2015.csv"

  def getResource(file: String): String = {
    new File(getClass.getResource(file).toURI).getPath
  }

  def getStationsRRD(stationFile: String): RDD[(StationId, Location)] = {
    val path = getResource(stationFile)
    val distFile = sc.textFile(path)
    distFile
      .map(line => line.split(","))
      .filter(p => p.length == 4 && !p(2).isEmpty && !p(3).isEmpty)
      .map(parts => (StationId(parts), Location(parts(2).toDouble, parts(3).toDouble)))
  }

  def getStationsRRDOld(stationFile: String): RDD[Station] = {
    val path = getResource(stationFile)
    println(path)
    val distFile = sc.textFile(path)
    distFile
      .map(line => line.split(","))
      .filter(p => p.length == 4 && !p(2).isEmpty && !p(3).isEmpty)
      .map(parts => Station(parts))
  }

  def getTemperatureRDD(temperatureFile: String): RDD[TemperatureRecord] = {
    val rdd = sc.textFile(getResource(temperatureFile))
    rdd.map(line => line.split(",")).filter(p => p.length == 5).map(parts => TemperatureRecord(
      StationId(parts),
      parts(2).toInt,
      parts(3).toInt,
      (parts(4).toDouble - 32) * 5 / 9
    ))
  }

  def locateTemperaturesRDD(year: Int, stationsFile: String, temperaturesFile: String): RDD[(Location, Double)] = {
    val stations: RDD[(StationId, Location)] =
      getStationsRRD(stationsFile)
    val temperatures: RDD[(StationId, Iterable[TemperatureRecord])] =
      getTemperatureRDD(temperaturesFile).groupBy(_.id)

    temperatures
      .mapValues(records => (records.foldLeft(0.0)((acc, v) => acc + v.temp), records.size))
      .join(stations)
      .values
      .groupBy(_._2)
      .mapValues(p => {
        val r = p.foldLeft((0.0, 0))((acc, current) => current match {case ((temp, count), _) => (acc._1 + temp, acc._2 + count)})
        r._1 / r._2
      })
  }
}
