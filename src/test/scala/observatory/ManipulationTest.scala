package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {
  test("make simple grid") {
    val grid = Manipulation.makeGrid(List((Location(0, 0), 10.0)))

    assert(grid(0, 0) == 10.0)
  }
}