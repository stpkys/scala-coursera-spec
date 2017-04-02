
def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x1 :: xs2, y1 :: ys1) =>
      if (x1 < y1) x1 :: merge(xs2, ys)
      else y1 :: merge(xs, ys1)
  }


merge(List(1, 3, 5), List(2, 4, 6))

Ordering.Int.lt(10, 5)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (h, t) = xs.span(y => x == y)
    h :: pack(t)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (t => (t.head, t.length))
}

val a = List("a", "a", "a", "b", "c", "c", "a")
pack(List("a", "a", "a", "b", "c", "c", "a"))
encode(a)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (el, res) => f(el) :: res)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, acc) => acc + 1 )

lengthFun(List[Int](1, 2, 3, 4))

mapFun(List[Int](1, 4, 6), (a: Int) => a * 2)

val t = for ( i <- List(1, 2, 3); j <- 1 to i) yield (i, j)

val k1 = Map('a' -> 1, 'b' -> 2, 'd' -> 4)
val k2 = Map('a' -> 1, 'c' -> 2)

List((1, 2), (1, 5), (4,5)).toMap



