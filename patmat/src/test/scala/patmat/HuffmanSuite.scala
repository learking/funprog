package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Leaf('k', 5)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("times"){
    assert(times("aabbbccccd".toList) === List(('b',3), ('d',1), ('a',2), ('c',4)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("iter for decode") {
    new TestTrees {
      assert(iter(t1, List(1,0,1)) === (List('b'),List(0, 1)))
    }
  }


  test("decode"){
    new TestTrees {
      assert(decode(t1, List(1,0,0,1)) === List('b', 'a', 'a', 'b'))
    }
  }

  test("encode"){
    new TestTrees {
      assert(encodeChar(t2, 'b', List()) === List(0, 1))
      assert(encode(t2)(List('b','a')) === List(0, 1, 0, 0))
    }
  }


    test("decode and encode a very short text should be identity") {
      new TestTrees {
        assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      }
    }

  test("convert"){
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)), ('b',List(1))))
    }
  }

  test("addPre"){
    assert(addPre(List(('a', List(1,0,0))), 1) === List(('a',List(1, 1, 0, 0))))
  }

  test("mergeCodeTables"){
    assert(mergeCodeTables(List(('a', List(2))), List(('b', List(3)))) === List(('a',List(0, 2)), ('b',List(1, 3))))
  }

  test("quickencode"){
    new TestTrees {
      assert(encode(t1)("aaaabbb".toList) === quickEncode(t1)("aaaabbb".toList))
    }
  }

}
