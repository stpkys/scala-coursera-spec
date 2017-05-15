package observatory

case class Location(lat: Double, lon: Double)

case class LocationsRad(lat: Double, lon: Double, latSin: Double, latCos: Double)

case class StationId(stn: Int, wban: Int)

object StationId {
  def apply(l: Seq[String]): StationId = StationId(
    if (l.head != "") l.head.toInt else 0,
    if (l(1) != "") l(1).toInt else 0
  )
}

case class Station(id: StationId, location: Location)

object Station {
  def apply(l: Array[String]): Station = Station(
    id = StationId(l),
    location = Location(l(2).toDouble, l(3).toDouble)
  )
}

case class TemperatureRecord(id: StationId, month: Int, day: Int, temp: Double)

case class Color(red: Int, green: Int, blue: Int)

