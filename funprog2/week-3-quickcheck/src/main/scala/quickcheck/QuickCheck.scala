package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty), for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("two elements") = {
    val h = insert(2, insert(1, empty))
    findMin(h) == 1
  }

  property("insert and remove one element") = {
    isEmpty(deleteMin(insert(1, empty)))
  }

  property("ordered") = forAll((h: H) => {
    val l = heapToList(h)

    def ordered(l: List[Int]): Boolean = {
      l match {
        case Nil => true
        case x :: Nil => true
        case x :: y :: tail => (x <= y) && ordered(tail)
      }
    }

    ordered(l)
  })

  property("meld two random seq") = forAll( (s1: List[Int], s2: List[Int]) => {
    val h1 = s1.foldLeft(empty)((h: H, i: Int) => insert(i, h))
    val h2 = s2.foldLeft(empty)((h: H, i: Int) => insert(i, h))
    val h = meld(h1, h2)
    val s = (s1 ++ s2).sorted
    heapToList(h) == s
  })

  def heapToList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: heapToList(deleteMin(h))
  }
}
