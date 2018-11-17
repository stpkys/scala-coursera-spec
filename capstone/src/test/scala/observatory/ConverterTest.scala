package observatory

import org.scalatest.FunSuite

class ConverterTest extends FunSuite {
  test("convert index to Lat") {
    assert(Converter.latFromIndex(0) === 90)
    assert(Converter.latFromIndex(1) === 90)
    assert(Converter.latFromIndex(360 * 180 - 1) === -89)
    assert(Converter.latFromIndex(360 * 180 / 2) === 0)
  }

  test("convert index to Lon") {
    assert(Converter.lonFromIndex(0) === -180)
    assert(Converter.lonFromIndex(1) === -179)
    assert(Converter.lonFromIndex(360 * 180 - 1) === 179)
    assert(Converter.lonFromIndex(360 * 180 / 2 + 180) === 0)
  }

  test("convert Location to index") {
    assert(Converter.toIndex(90, -180) === 0)
    assert(Converter.toIndex(90, -179) === 1)
    assert(Converter.toIndex(89, -180) === 360)
    assert(Converter.toIndex(-89, 179) === 360 * 180 - 1)
  }
}
