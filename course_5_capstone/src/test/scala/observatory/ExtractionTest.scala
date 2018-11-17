package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  test("parse line of csv file") {
    val parts = ",91004,+80.000,-113.000".split(",")
    assert(parts.length === 4)
  }

  test("create station id") {
    assert(StationId(List("123", "")) === StationId(123, 0))
    assert(StationId(Array("007018", "", "+00.000", "+000.000")) === StationId(7018, 0))
  }
}