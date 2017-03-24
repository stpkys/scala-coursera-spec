class Rational(x: Int, y: Int) {
//  private val g = gcd(x, y)
  def numer: Int = x
  def denom: Int = y

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def +(that: Rational): Rational = {
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)
  }


  override def toString: String = {
    val g = gcd(numer, denom)
    (numer / g) + "/" + (denom / g)
  }

  def neg: Rational = new Rational(-numer, denom)
}

val t = new Rational(1, 2)
t.numer
t.+(new Rational(3, 4))
t.neg.hashCode()
t.neg.hashCode()


