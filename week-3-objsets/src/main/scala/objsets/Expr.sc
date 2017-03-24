
trait Expr

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object Expr {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) => e1 match {
      case Sum(_, _) => "(" + show(e1) + ") * " + show(e2)
      case Number(_) => show(e1) + " * " + show(e2)
    }
  }
}


Expr.eval(Sum(Prod(Number(2), Number(2)), Number(3)))
Expr.eval(Prod(Sum(Number(3), Number(2)), Number(2)))

Expr.show(Sum(Prod(Number(2), Number(2)), Number(3)))
Expr.show(Prod(Sum(Number(3), Number(2)), Number(2)))

Expr.eval(Sum(Number(1), Number(22)))
Expr.show(Sum(Number(1), Number(22)))
Expr.eval(Prod(Number(2), Number(11)))
Expr.show(Prod(Number(2), Number(11)))


val t = 100 :: List(1, 2, 3, 4, 5)
t.head
