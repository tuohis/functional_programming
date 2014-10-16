package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("pascal: col=0 row=4") {
    assert(pascal(0,4) === 1)
  }
  
  test("pascal: col=1 row=4") {
    assert(pascal(1,4) === 4)
  }
  
  test("pascal: col=2 row=4") {
    assert(pascal(2,4) === 6)
  }
  
  test("pascal: col=3 row=4") {
    assert(pascal(3,4) === 4)
  }
  
  test("pascal: col=4 row=4") {
    assert(pascal(4,4) === 1)
  }
  
  test("pascal: negative row") {
    try {
      pascal(0, -1)
      assert(false)
    } catch {
      case e: java.util.NoSuchElementException => assert(true)
    }
  }
  
  test("pascal: negative col") {
    try {
      pascal(-2, 0)
      assert(false)
    } catch {
      case e: java.util.NoSuchElementException => assert(true)
    }
  }
  
  test("pascal: both negative row and col") {
    try {
      pascal(-5, -1)
      assert(false)
    } catch {
      case e: java.util.NoSuchElementException => assert(true)
    }
  }
  
  test("pascal: too big col") {
    try {
      pascal(7, 6)
      assert(false)
    } catch {
      case e: java.util.NoSuchElementException => assert(true)
    }
  }
}
