package scalaexercises.tests

import scalaexercises.chapter02
import org.scalatest.FunSuite

/**
 * Created by Papa on 08.12.2014.
 */
class chapter02Test extends FunSuite {
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
