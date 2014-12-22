package scalaexercises.tests

import org.scalatest.ShouldMatchers

import scalaexercises.chapter03
import org.scalatest.FunSuite

// Exercise 3.2
class chapter03Test extends FunSuite with ShouldMatchers{
  val ch = new chapter03

  test("tail function returns list tail")  {
    ch.tail(List(1, 2, 3)) should equal (List(2, 3))
  }

  test("setHead function replaces the first element") {
    ch.setHead(List(1, 2, 3), 4) should equal (List(4, 2, 3))
  }
}
