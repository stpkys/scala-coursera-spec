package quickcheck

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap
object QuickCheckBogus1BinomialHeap extends QuickCheckHeap with Bogus1BinomialHeap
object QuickCheckBogus2BinomialHeap extends QuickCheckHeap with Bogus2BinomialHeap
object QuickCheckBogus3BinomialHeap extends QuickCheckHeap with Bogus3BinomialHeap
object QuickCheckBogus4BinomialHeap extends QuickCheckHeap with Bogus4BinomialHeap
object QuickCheckBogus5BinomialHeap extends QuickCheckHeap with Bogus5BinomialHeap

@RunWith(classOf[JUnitRunner])
class QuickCheckSuite extends FunSuite with Checkers {
  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }

  test("Binomial heap satisfies properties.") {
    check(new QuickCheckHeap with quickcheck.test.BinomialHeap)
  }

  test("Bogus (1) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus1BinomialHeap)
  }

  test("Bogus (2) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus2BinomialHeap)
  }

  test("Bogus (3) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus3BinomialHeap)
  }

  test("Bogus (4) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap)
  }

  test("Bogus (5) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus5BinomialHeap)
  }
}
