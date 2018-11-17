package recfun

import scala.annotation.tailrec

object Main {

  def factorial(n: => Int): Int = {
    @tailrec
    def calculate(result: Int, current: Int): Integer = {
      if (current == 1) result else calculate(result * n, n - 1)
    }

    calculate(1, n)
  }

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def sum1(f: Int => Int)(a: Int, b: Int): Int = {
    //    def sumf(a: Int, b: Int): Int = {
    //      if (a > b) 0
    //      else f(a) + sumf(a+1, b)
    //    }
    //    return sumf
    if (a > b) 0 else f(a) + sum1(f)(a + 1, b)
  }

  def genFunc(comb: (Int, Int) => Int, base: Int)(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) base
    else comb(f(a), genFunc(comb, base)(f)(a + 1, b))

  def main(args: Array[String]) {
    def prod(a: Int, b: Int) = genFunc((x: Int, y: Int) => x * y, 1)((d) => d)(a, b)

    def factorial(n: Int): Int = product(x => x)(1, n)

    println(product(x => x)(1, 5))
    println(prod(1, 5))
    println(factorial(4))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c < 1 || c >= r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def test(current: List[Char], count: Int): Boolean = {
      if (current.isEmpty) count == 0
      else if (count < 0) false
      else if (current.head == '(') test(current.tail, count + 1)
      else if (current.head == ')') test(current.tail, count - 1)
      else test(current.tail, count)
    }

    test(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
