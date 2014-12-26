package scalaexercises

// Chapter 3
final class chapter03 {

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("list cannot be empty")
    case h :: t => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("list cannot be empty")
    case h :: t => newHead :: t
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0)
      l
    else
      l match {
        case Nil => throw new IllegalArgumentException("list cannot be empty")
        case h :: t => drop(t, n-1)
      }

  // Exercise 3.5
  def dropWhile[A](l: List[A])(predicate: A => Boolean): List[A] = l match {
    case Nil => Nil
    case h :: t =>
      if (predicate(h))
        dropWhile(t)(predicate)
      else
        l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("list cannot be empty")
    case h :: Nil => Nil
    case h :: t => h :: init(t)
  }

  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B) : B =
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs,z)(f))
    }

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a: A, i: Int) => i+1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z:B)(f: (B,A) => B):B =
    as match {
      case Nil => z
      case x::t => foldLeft(t, f(z, x))(f)
    }

  // Exercise 3.11
  def sumLeft(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Int]): Int =
    foldLeft(as, 1)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((i: Int, a: A) => i+1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc: List[A], a: A) => a :: acc)

  // Exercise 3.13
  def foldRightWithLeft[A,B](as: List[A], z:B)(f: (A,B) => B) : B = {
    val rev =foldLeft(as, List[A]())((acc: List[A], a: A) => a :: acc)
    foldLeft(rev, z)((c, d) => f(d, c))
  }
}
