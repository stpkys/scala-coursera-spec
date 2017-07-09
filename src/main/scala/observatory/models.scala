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

object ColorScales {
  val scale1: List[(Double, Color)] = List[(Double, Color)](
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  val scale2: List[(Double, Color)] = List[(Double, Color)](
    (7, Color(0, 0, 0)),
    (4, Color(255, 0, 0)),
    (2, Color(255, 255, 0)),
    (0, Color(255, 255, 255)),
    (-2, Color(0, 255, 255)),
    (-7, Color(0, 0, 255))
  )
}
