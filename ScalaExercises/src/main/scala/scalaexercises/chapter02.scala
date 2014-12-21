package scalaexercises

class chapter02 {

  // Exercise 2.1
  def fib(n: Int): Int =
    if (n == 0)
      0
    else if (n == 1)
      1
    else fib(n - 2) + fib(n - 1)

  // Exercise 2.2
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {

    def check(prevElem: A, i: Int): Boolean = {
      if (i >= as.length)
        true
      else if (!ordered(prevElem, as(i)))
        false
      else
        check(as(i), i + 1)
    }

    if (as.length == 0)
      true
    else
      check(as(0), 1)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a:A, b:B) => f(a)(b)

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))
}