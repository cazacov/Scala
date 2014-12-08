package scalaexercises

class chapter02 {
  // Exercise 2.1

  def fib(n: Int): Int =
    if (n == 0)
      0
    else if (n == 1)
      1
    else fib(n-2) + fib(n-1)
}