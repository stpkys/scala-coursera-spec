package observatory

/**
  * Created by viktor on 27/06/2017.
  *
  */
object Converter {
  def latFromIndex(index: Int): Int = {
    90 - index / 360
  }

  def lonFromIndex(index: Int): Int = {
    -180 + index % 360
  }

  def toIndex(lat: Int, lon: Int): Int = {
    (90 - lat) * 360 + (180 + lon)
  }
}
