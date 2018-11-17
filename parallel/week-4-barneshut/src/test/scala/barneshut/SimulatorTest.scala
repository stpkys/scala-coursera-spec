package barneshut

import barneshut.BarnesHut.model
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.parallel.{TaskSupport, defaultTaskSupport}

@RunWith(classOf[JUnitRunner])
class SimulatorTest extends FunSuite {
  test("test compute boundaries") {
    val bodies = Seq(new Body(0, 0, 0, 0, 0), new Body(0, 10, 10, 0, 0))
    val simulator = new Simulator(defaultTaskSupport, new TimeStatistics)
    val b = simulator.computeBoundaries(bodies)
    assert(b.minX === 0)
    assert(b.minY === 0)
    assert(b.maxX === 10)
    assert(b.maxY === 10)
  }
}
