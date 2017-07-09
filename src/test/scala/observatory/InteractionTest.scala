package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  test("tile location top left corner") {
    assertLocation(Location(85.0511, -180), Interaction.tileLocation(8, 0, 0))
    assertLocation(Location(-85.0511, -180), Interaction.tileLocation(8, 0, 256))
    assertLocation(Location(85.0511, 180), Interaction.tileLocation(8, 256, 0))
    assertLocation(Location(0, 0), Interaction.tileLocation(8, 128, 128))
    assertLocation(Location(-85.0511, 180), Interaction.tileLocation(8, 256, 256))
  }

  def assertLocation(expected: Location, current: Location, eps: Double = 1e-3): Unit = {
    assert(math.abs(expected.lat - current.lat) <= eps, s"Latitude ${current.lat} instead of ${expected.lat}")
    assert(math.abs(expected.lon - current.lon) <= eps, s"Lon ${current.lon} instead of ${expected.lon}")
  }
}
