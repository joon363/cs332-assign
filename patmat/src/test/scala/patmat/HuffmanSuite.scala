package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t0 = Leaf('a',1)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)),Leaf('t', 2))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  test("weight of a single tree") {
    new TestTrees {
      assert(weight(t0) === 1)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  test("char of a single tree") {
    new TestTrees {
      assert(chars(t0) === List('a'))
    }
  }
  test("makeCodeTree test") {
    new TestTrees {
      assert(weight(t3) === 4)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  test("times test for some List") {
    assert(times(List('a','b','a','a'))===List(('a', 3), ('b', 1)))
  }
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  test("singleton test") {
    new TestTrees {
      assert(singleton(List(t1))=== true)
      assert(singleton(List(t1,t2))=== false)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree Test") {
    assert(createCodeTree(List('a','b','a'))=== Fork(Leaf('b',1),Leaf('a',2),List('b','a'), 3))
    assert(createCodeTree(List('a','b','c','b'))=== Fork(Fork(Leaf('a',1),Leaf('c',1),List('a','c'),2),Leaf('b',2),List('a','c','b'), 4))
  }
  test("decode secret Test") {
    assert(decodedSecret===string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  test("decode test") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === List('a','b'))
    }
  }
  test("encode test") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }
  test("codeBits test") {
    val table = List(('a',List(0)),('b',List(0,1)),('c',List(1,1)))
    assert(codeBits(table)('c')===List(1,1))
    assert(codeBits(table)('a')===List(0))
  }

  test("convert tree test") {
    new TestTrees {
      assert(convert(t0) === List(('a',List(0))))
    }
  }
  test("convert tree test 2") {
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)),('b',List(1))))
    }
  }
  test("convert tree test 3") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0,0)),('b',List(0,1)),('d',List(1))))
    }
  }
  test("quickEncode test") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0,1))
    }
  }
}
