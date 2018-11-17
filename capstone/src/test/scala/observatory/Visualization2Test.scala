package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  test("bilinear interpolation") {
    val result = Visualization2.bilinearInterpolation(0.5, 0.2, 91, 162, 210, 95)
    assert(result == 146.1)
  }

}
