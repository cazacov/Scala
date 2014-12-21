package scalaexercises.tests

import scalaexercises.chapter02
import org.scalatest.FunSuite

// Exercise 2.1
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

// Exercise 2.2
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

// Exercise 2.3
class chapter02Exercise23 extends FunSuite {
  val ch = new chapter02

  test("curry can generate multiplier functions") {

    val multiplier = ch.curry((x: Int, y: Int) => x * y)
    val tenTimes: (Int) => Int = multiplier(10)

    assert(tenTimes(1) == 10)
    assert(tenTimes(3) == 30)
  }

  test("curry can generate addition functions") {

    val adder = ch.curry((x: Int, y: Int) => x + y)
    val increment: (Int) => Int = adder(1)

    assert(increment(1) == 2)
    assert(increment(41) == 42)
  }
}

// Exercise 2.5
class chapter02Exercise25 extends FunSuite {
  val ch = new chapter02

  test("compose can convert grad to radian and then calculate Sin") {

    val gradToRadian = (a: Double) => a * Math.PI / 180.0
    val sin = (a: Double) => Math.sin(a)

    val sinOfGrad = ch.compose(sin, gradToRadian)

    val sinOf30 = sinOfGrad(30)

    assert(Math.abs(sinOf30 - 0.5) < 0.000000001)

    // Test how the andThen function is working
    val scalaSinOfGrad = gradToRadian andThen sin
    assert(Math.abs(scalaSinOfGrad(30) - 0.5) < 0.000000001)
  }

}

