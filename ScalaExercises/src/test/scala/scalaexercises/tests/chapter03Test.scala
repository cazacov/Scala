package scalaexercises.tests

import org.scalatest.ShouldMatchers

import scalaexercises.chapter03
import org.scalatest.FunSuite

// Exercise 3.2
class chapter03Exercise32Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("tail function returns list tail") {
    ch.tail(List(1, 2, 3)) should equal(List(2, 3))
  }

  test("tail function throws an execption if the list is empty") {
    an[IllegalArgumentException] should be thrownBy ch.tail(List())
  }
}

// Exercise 3.3
class chapter03Exercise33Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("setHead function replaces the first element") {
    ch.setHead(List(1, 2, 3), 4) should equal(List(4, 2, 3))
  }

  test("setHead throws an exception if the list is empty") {
    an[IllegalArgumentException] should be thrownBy ch.setHead(List(), 42)
  }
}

// Exercise 3.4
class chapter03Exercise34Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("drop skip first n elements") {
    ch.drop(List(1, 2, 3, 4, 5, 6), 3) should equal (List(4, 5, 6))
  }

  test("drop throws an exception if list is shorter than n") {
    an [IllegalArgumentException] should be thrownBy ch.drop(List(1, 2), 3)
  }
}

// Exercise 3.5
class chapter03Exercise35Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("dropWhile skips elements while predicate is true") {
    ch.dropWhile(List(1, 2, 3, 4, 5, 6))(x => x < 4) should equal (List(4, 5, 6))
  }

  test("dropWhile returns an empty list if the predicate is always true") {
    ch.dropWhile(List(1, 2, 3, 4, 5, 6))(x => true) should equal (List())
  }

  test("dropWhile returns an empty list if the input list is empty") {
    ch.dropWhile(List())(x => true) should equal (List())
  }
}


// Exercise 3.6
class chapter03Exercise36Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("init returns all list elements but the last one") {
    ch.init(List(1, 2, 3, 4)) should equal (List(1,2,3))
  }

  test("init throws an exception if the list is empty") {
    an [IllegalArgumentException] should be thrownBy ch.init(List())
  }
}


// Exercise 3.9
class chapter03Exercise39Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("length returns 0 for an empty list") {
    ch.length(List()) should equal (0)
  }

  test("length returns list length") {
    ch.length(List(1,2,3)) should equal (3)
  }
}
