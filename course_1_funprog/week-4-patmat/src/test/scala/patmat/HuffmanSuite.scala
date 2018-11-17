package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list more") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 1), Leaf('x', 1))
    assert(combine(leaflist) === List(Leaf('x', 1), Fork(Leaf('e', 1), Leaf('t', 1), List('e', 't'), 2)))
  }

  test("test until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) == List(
      Fork(
        Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),
        Leaf('x', 4),
        List('e', 't', 'x'),
        7
      )))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  val secretWord = "huffmanestcool"
  test("decode and encode a longer text should be identity") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)(secretWord.toList)) === secretWord.toList)
    }
  }

  test("secret text decode should be correct") {
    assert(decodedSecret.mkString === secretWord)
  }

  test("secret text encode should be same as secret") {
    assert(encode(frenchCode)(string2Chars(secretWord)) === secret)
  }

  test("quick encoding should work on short text") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quick encoding should work on secret") {
    assert(quickEncode(frenchCode)(string2Chars(secretWord)) === secret)
  }

}
