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
    val array = Array[Int]()
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(actual)
  }

  test("isSorted on one-element array returns true") {
    val array = Array(1)
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(actual)
  }

  test("isSorted on sorted array returns true") {
    val array = Array(1, 2, 3)
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(actual)
  }

  test("isSorted on unsorted array returns false") {
    val array = Array(1, 3, 2)
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(!actual)
  }

  test("isSorted on sorted char array returns true") {
    val array = "abcdef".toCharArray
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(actual)
  }

  test("isSorted on unsorted char array returns false") {
    val array = "scala".toCharArray
    val actual = ch.isSorted(array)((x, y) => x < y)
    assert(!actual)
  }

}
