package scalaexercises.tests

import scalaexercises.chapter02
import org.scalatest.FunSuite

class chapter02Exercise21 extends FunSuite {
  test("Fib(0) returns 0") {
    val ch = new chapter02
    val actual = ch.fib(0)
    val expected = 0
    assert(expected == actual)
  }

  test("Fib(1) returns 1") {
    val ch = new chapter02
    val actual = ch.fib(1)
    val expected = 1
    assert(expected == actual)
  }

  test("Fib(7) returns 13") {
    val ch = new chapter02
    val actual = ch.fib(7)
    val expected = 13
    assert(expected == actual)
  }
}

class chapter02Exercise22 extends FunSuite {
  val ch = new chapter02

  test("isSorted on empty array returns true") {
    var array = Array[Int]()
    var actual = ch.isSorted(array, (x:Int, y:Int) => x < y)
    assert(true == actual)
  }

  test("isSorted on one-element array returns true") {
    var array = Array(1)
    var actual = ch.isSorted(array, (x:Int, y:Int) => x < y)
    assert(true == actual)
  }

  test("isSorted on sorted array returns true") {
    var array = Array(1, 2, 3)
    var actual = ch.isSorted(array, (x:Int, y:Int) => x < y)
    assert(true == actual)
  }

  test("isSorted on unsorted array returns false") {
    var array = Array(1, 3, 2)
    var actual = ch.isSorted(array, (x:Int, y:Int) => x < y)
    assert(false == actual)
  }

  test("isSorted on sorted char array returns true") {
    var array = "abcdef".toCharArray()
    var actual = ch.isSorted(array, (x:Char, y:Char) => x < y)
    assert(false == actual)
  }

  test("isSorted on unsorted char array returns false") {
    var array = "scala".toCharArray()
    var actual = ch.isSorted(array, (x:Char, y:Char) => x < y)
    assert(false == actual)
  }

}
