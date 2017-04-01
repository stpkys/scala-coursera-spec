package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import reductions.ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for empty string parallel") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 2) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("strings") {
    assert(balance("(if (zero? x) max (/ 1 x))".toArray) === true)
    assert(balance("I told him (that it's not (yet) done). (But he wasn't listening)".toArray) === true)
    assert(balance("())(".toArray) === false)
  }


  test("strings parallel") {
    def check(input: String, expected: Boolean) = {
      assert(parBalance(input.toArray, 1) === expected, s"balance($input) should be $expected")
      assert(parBalance(input.toArray, 2) === expected, s"balance($input) should be $expected")
      assert(parBalance(input.toArray, 5) === expected, s"balance($input) should be $expected")
      assert(parBalance(input.toArray, 500) === expected, s"balance($input) should be $expected")
    }

    check("((abcdef))", true)
    check("(if (zero? x) max (/ 1 x))", true)
    check("I told him (that it's not (yet) done). (But he wasn't listening)", true)
    check("())(", false)
    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
    check("", true)
  }


}