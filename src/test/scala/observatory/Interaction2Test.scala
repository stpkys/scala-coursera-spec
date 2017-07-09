package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Interaction2Test extends FunSuite with Checkers {

  test("slider stuff") {
    val layer = Var(Layer(LayerName.Temperatures, ColorScales.scale1, 2000 to 2005))
    val t = Var(10)
    val result = Interaction2.yearSelection(layer, t)
    assert(result() === 2000)
    t() = 2010
    assert(result() === 2005)
    layer() = Layer(LayerName.Temperatures, ColorScales.scale1, 2000 to 2003)
    assert(result() === 2003)
  }

  test("layer url pattern") {
    val path = Interaction2.layerUrlPattern(Var(Layer(LayerName.Temperatures, ColorScales.scale1, 2000 to 2005)), Signal(2003))
    assert(path().endsWith("target/temperatures/2003/{z}/{x}-{y}.png"))
  }
}
