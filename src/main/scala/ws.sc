// -------------------------
// Task 1
// -------------------------
object Maybe {
  def some[A](a: A): Maybe[A] = new Maybe[A] {
    def pick[B](n: => B, s: A => B): B = s(a)
  }
  def none[A]: Maybe[A] = new Maybe[A] {
    def pick[B](n: => B, s: A => B): B = n
  }
}

trait Maybe[+A] {
  import Maybe._

  def pick[B](none: => B, some: A => B): B

  def map[B](f: A => B): Maybe[B] =
       pick(none, a => some(f(a)))

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = pick(none, a => f(a))

  def getOrElse[B >: A](b: => B): B = pick(b, a => a)

  def ap[B](fab: Maybe[A => B]): Maybe[B] = fab flatMap map
}


import Maybe._
val plusTwo: Int => Int = _ + 2
val x = some(5).map(plusTwo).getOrElse(0)            // 7
val y = none.map(plusTwo).getOrElse(0)               // 0
val z = some(7).ap(Maybe.some(plusTwo)).getOrElse(0) // 9


// -------------------------
// Task 2
// -------------------------

trait MyList[+A] {
  import MyList._

  def fold[B](k: Option[(A, B)] => B): B

  def map[B](f: A => B): MyList[B] =
    fold[MyList[B]]({
      case None => nil[B]
      case Some((h, t)) => cons[B](f(h), t)
    })

  def flatMap[B](f: A => MyList[B]): MyList[B] = fold[MyList[B]] {
    case None => nil[B]
    case Some((h,t)) => f(h).fold[MyList[B]] {
      case None => t
      case Some((h2, t2)) => cons(h2, t2)
    }
  }

  def headOption: Option[A]                    =
    fold[Option[A]](k => k.map({case (h, _) => h}))

  def tailOption: Option[MyList[A]]            =
    fold[Option[(MyList[A], MyList[A])]]({
      case None => Some((nil, nil))
      case Some((h, t)) => t.map(tails => (cons(h, tails._1), tails._1))
    }).map(_._2)

  def isEmpty: Boolean                         =
    fold[Boolean]({
      case None => true
      case Some(_) => false
    })

  def length: Integer                          =
    fold[Integer]({
      case None => 0
      case Some((_, tailSize)) => tailSize + 1
    })

  def toList: List[A] = fold[List[A]]({
    case None => List.empty
    case Some((h, t)) => h :: t
  })
}

object MyList {
  def nil[A] = new MyList[A] {
    def fold[B](k: Option[(A, B)] => B): B = k(None)
  }

  def cons[A](h: A, t: MyList[A]) = new MyList[A] {
    def fold[B](k: Option[(A, B)] => B): B = k(Some((h, t.fold(k))))
  }
}

import MyList._

val x0 = cons(5, cons(8, nil))
val x1 = cons(5, cons(8, nil)).map(_ + 2)
nil.isEmpty
x0.toList

val x2 = x1.flatMap(k => cons(k, cons(k + 10, nil)))
x1.length
x2.length
x2.toList

x1.headOption // Some(7)
x0.tailOption.map(t => t.length)
x1.tailOption.flatMap(t => t.headOption)
x2.tailOption.map(t => t.toList)
val y1 = cons(7, cons(8, cons(3, cons(1, nil)))).length // 4