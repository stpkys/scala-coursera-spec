package observatory


import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("visualisation") {
    Visualization.visualize(List(
      (Location(0,0), 30),
      (Location(50,50), 30),
      (Location(-50,-50), 30)
    ), Visualization.scale)
  }

  test("interpolate color") {
    import Visualization._
    assert(interpolateColor(scale, 12) === Color(255, 255, 0))
    assert(interpolateColor(scale, 22) === Color(255, 128, 0))
    assert(interpolateColor(scale, 32) === Color(255, 0, 0))
    assert(interpolateColor(scale, 46) === Color(255, 128, 128))
    assert(interpolateColor(scale, -100) === Color(0, 0, 0))
    assert(interpolateColor(scale, 100) === Color(255, 255, 255))
  }

  test("distance between two dots") {
    import Visualization._
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.1)

    assert(distance(Location(10, 10), Location(10, 10)) === 0.0)
    assert(distance(Location(10, 10), Location(10.1, 10.1)) === 15.6)
    assert(distance(Location(9, 10), Location(11, 12)) === distance(Location(11, 12), Location(9, 10)))
  }
}
