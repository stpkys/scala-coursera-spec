abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true
  override def predecessor: Nat = throw new Error()
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if (that.isZero) this else throw new Error()
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def +(that: Nat): Nat = new Succ(n + that)
  override def -(that: Nat): Nat = if (that.isZero) n else n - that.predecessor
}

val i1 = new Succ(Zero)
val i2 = new Succ(i1)

(i2 - i1).isZero


