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

// Exercise 3.10
class chapter03Exercise10Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("foldLeft returns z for an empty list") {
    ch.foldLeft(List[Int](), 42)(_ + _) should equal (42)
  }

  test("foldLeft can be used to sum values in the list") {
    ch.foldLeft((1 to 100).toList, 0)(_ + _) should equal (5050)
  }
}


// Exercise 3.11
class chapter03Exercise11Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("sumLeft works as expected") {
    ch.sumLeft((1 to 100).toList) should equal (5050)
  }

  test("productLeft works as expected") {
    ch.productLeft(List(1,2,3,4,5,6)) should equal (720)
  }

  test("lengthLeft works as expected") {
    ch.lengthLeft((1 to 100000).toList) should equal (100000)
  }
}


// Exercise 3.12
class chapter03Exercise12Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("reverse works as expected") {
    ch.reverse(List(1,2,3)) should equal (List(3,2,1))
  }

  test("reverse of empty list is an empty list") {
    ch.reverse(List()) should equal (List())
  }
}

// Exercise 3.12
class chapter03Exercise13Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("foldRight can be expressed with foldLeft") {
    ch.foldRightWithLeft("abcde".toList, "")((ch: Char, acc:String) => acc + ch) should equal ("edcba")
  }
}

// Exercise 3.14
class chapter03Exercise14Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("append works as expected") {
    ch.append("abc".toList, "def".toList) should equal ("abcdef".toList)
  }

  test("append of an empty list returns the first list") {
    ch.append(List(1,2,3), List()) should equal (List(1,2,3))
  }

  test("append to an empty list returns the second list") {
    ch.append(List(), List(1,2,3)) should equal (List(1,2,3))
  }
}

// Exercise 3.15
class chapter03Exercise15Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("concat of empty list returns an empty list") {
    ch.concat(List(List())) should equal (List())
  }

  test("concat of two empty lists returns an empty list") {
    ch.concat(List(List(), List())) should equal (List())
  }

  test("concat concatenates a list of lists into a single list") {
    ch.concat(List(List(1,2,3), List[Int](), List(4,5), List(6,7,8), List[Int]())) should equal (List(1,2,3,4,5, 6, 7,8))
  }
}

// Exercise 3.16
class chapter03Exercise16Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("incrementList increments every element of the list") {
    ch.incrementList(List(1,2,3)) should equal (List(2,3,4))
  }
}

// Exercise 3.18
class chapter03Exercise18Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("map function maps every element of the list into a new one") {
    ch.map(List(1,2,3))((x: Int) => 2 * x) should equal (List(2,4,6))
  }
}

// Exercise 3.19
class chapter03Exercise19Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("filter function works as expected") {
    ch.filter(List(1,2,3,4,5,6,7,8))((x: Int) => (x % 2 == 0)) should equal (List(2,4,6,8))
  }
}

// Exercise 3.20
class chapter03Exercise20Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("flatMap function works as expected") {
    ch.flatMap(List(1,2,3))(i => List(i,i)) should equal (List(1,1,2,2,3,3))
  }
}

// Exercise 3.21
class chapter03Exercise21Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("filter function can be implemented using flatMap") {
    ch.filterWithFlatMap(List(1,2,3,4,5,6,7,8))((x: Int) => (x % 2 == 0)) should equal (List(2,4,6,8))
  }
}

// Exercise 3.22
class chapter03Exercise22Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("zipInt adds integer lists that have same length") {
    ch.zipInts(List(1,2,3), List(4,5,6)) should equal (List(5,7,9))
  }

  test("zipInt can handle lists of different length") {
    ch.zipInts(List(1,2,3), List(4,5)) should equal (List(5,7,3))
  }

  test("zipInt can handle lists of different length 2") {
    ch.zipInts(List(1,2), List(4,5,6)) should equal (List(5,7,6))
  }
}

// Exercise 3.23
class chapter03Exercise23Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("zipWith can be used to add integer lists that have same length") {
    ch.zipWith(List(1,2,3), List(4,5,6))(_+_) should equal (List(5,7,9))
  }

  test("zipWith can be used to multiply integer lists that have same length") {
    ch.zipWith(List(1,2,3), List(4,5,6))(_*_) should equal (List(4,10,18))
  }

}

// Exercise 3.24
class chapter03Exercise24Test extends FunSuite with ShouldMatchers {
  val ch = new chapter03

  test("test hasSequence") {
    ch.hasSubsequence(List(1,2,3, 4), List(4,5,6)) should equal (false)
  }

  test("test hasSequence2") {
    ch.hasSubsequence(List(1,2,3,4), List(1,2)) should equal (true)
  }

  test("test hasSequence3") {
    ch.hasSubsequence(List(1,2,3,4), List(2,3)) should equal (true)
  }

  test("test hasSequence4") {
    ch.hasSubsequence(List(1,2,3,4), List(4)) should equal (true)
  }

  test("test hasSequence5") {
    ch.hasSubsequence(List(1,2,3,4), List(1,2,4)) should equal (false)
  }

  test("test hasSequence6") {
    ch.hasSubsequence(List(), List()) should equal (true)
  }

  test("test hasSequence7") {
    ch.hasSubsequence(List(), List(1)) should equal (false)
  }

  test("test hasSequence8") {
    ch.hasSubsequence(List(1), List()) should equal (true)
  }


}
