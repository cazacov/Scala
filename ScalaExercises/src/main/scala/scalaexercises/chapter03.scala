package scalaexercises

import scala.collection.immutable.List

// Chapter 3
class chapter03 {

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("list cannot be empty")
    case h :: t => t
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("list cannot be empty")
    case h :: t => newHead :: t
  }

}
