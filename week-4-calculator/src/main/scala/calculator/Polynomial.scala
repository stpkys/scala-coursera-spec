package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal({
      val b1 = b()
      b1 * b1 - 4 * a() * c()
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val d = delta()
      val bv = b()
      val av = a()
      val cv = c()
      if (d < 0 || (av == 0 && bv == 0)) Set()
      else if (av == 0) Set( -1.0 * cv / bv)
      else Set((-bv + math.sqrt(d)) / (2 * av), (-bv - math.sqrt(d)) / (2 * av))
    })
  }
}
