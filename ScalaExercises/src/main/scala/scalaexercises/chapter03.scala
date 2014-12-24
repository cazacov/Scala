package scalaexercises

import scala.collection.immutable.List
import scala.collection.immutable.Nil

// Chapter 3
class chapter03 {

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

}
