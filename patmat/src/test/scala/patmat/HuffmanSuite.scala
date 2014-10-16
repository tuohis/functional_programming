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
    val c1 = List[Char]('a', 'b', 'c', 'a', 'a', 'b', 'c', 'a', 'b', 'd')
    val s1 = "vesihiisi"
    val res1 = List(('v', 1), ('e', 1), ('s', 2), ('i', 4), ('h', 1))
    val secretAnswer = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val l1 = List[CodeTree](Leaf('a',2), Leaf('b',3), Leaf('c',4), Leaf('d',5), Leaf('e',6), Leaf('f',7), Leaf('g',8), Leaf('h',9))
    val l2 = (List[CodeTree](Fork(l1.head, l1.tail.head, chars(l1.head) ::: chars(l1.tail.head), weight(l1.head) + weight(l1.tail.head))) ::: l1.tail.tail).sortBy(weight)
    val l3 = (List[CodeTree](Fork(l2.head, l2.tail.head, chars(l2.head) ::: chars(l2.tail.head), weight(l2.head) + weight(l2.tail.head))) ::: l2.tail.tail).sortBy(weight)
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
  
  test("singleton of Nil") {
    assert(!singleton(Nil))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times(string2chars('vesihiisi')") {
    new TestTrees {
      val res = times(string2Chars(s1))
      assert(res.length === 5)
      assert(res.head._1 === 'v')
      assert(res.head._2 === 1)
      assert(res === res1)
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    
  }
  
  test("combine of Nil") {
    assert(combine(Nil) === Nil)
  }
  
  test("combine of a more advanced leaf list") {
    new TestTrees {
      assert(combine(l1) === l2)
      assert(combine(l2) === l3)
      assert(combine(combine(l1)) === l3)
    }
    
  }
  
  test("createCodeTree of 'sometextvesihiisi'") {
    val tree = createCodeTree("sometextvesihiisi".toList)
    assert(!chars(tree).isEmpty)
  }
  
  test("Decode the secret message") {
    new TestTrees {
      assert(decodedSecret === secretAnswer)
    }
  }
  
  test("Encode the secret message") {
    new TestTrees {
      val encoded = encode(frenchCode)(secretAnswer)
      assert(encoded === secret)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("Encode the secret message using quickEncode") {
    new TestTrees {
      val encoded = quickEncode(frenchCode)(secretAnswer)
      assert(encoded === secret)
    }
  }
}
