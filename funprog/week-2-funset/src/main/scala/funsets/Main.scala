package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))
  val u = union(singletonSet(1), singletonSet(2))
  println(contains(u, 1), contains(u, 2), contains(u,3))

  printSet(u)
  val even = union(singletonSet(2), singletonSet(4))
  printSet(even)
  val newSet = map(even, x => x * 3)
  printSet(newSet)

  def select(l: List[Int], n: Int): Int = {
    if (l.isEmpty) throw new IndexOutOfBoundsException("out of bound")
    else if (n == 0) l.head
    else select(l.tail, n - 1)
  }
}
