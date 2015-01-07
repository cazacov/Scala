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

  // Exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    // Some optimization
    if (a1.isEmpty)
      a2
    else if (a2.isEmpty)
      a1
    else
      // Append expressed in terms of foldRight
      foldRight(a1, a2)((a, acc) => a :: acc)

  // Exercise 3.15
  def concat[A](ass: List[List[A]]): List[A] =
    reverse(foldLeft(ass, List[A]())((acc, list) => foldLeft(list, acc)((ac, elem) => elem :: ac)))

  // Exercise 3.16
  def incrementList(as: List[Int]): List[Int] =
    foldRight(as, List[Int]())((i: Int, acc: List[Int]) => (i+1):: acc)

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A=>B): List[B] =
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case x :: xs =>
        if (f(x))
          x :: filter(xs)(f)
        else
          filter(xs)(f)
    }

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case x :: xs =>
        f(x) ++ flatMap(xs)(f)
    }

  // Exercise 3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)( (elem:A) => if (f(elem)) List(elem) else List[A]())


  // Exercise 3.22
  def zipInts(as: List[Int], bs: List[Int]):List[Int] =
    as match {
      case Nil => bs
      case ah :: at =>
        bs match {
          case Nil => as
          case bh :: bt =>
            (ah + bh) :: zipInts(at, bt)
        }
    }

  // Exercise 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C):List[C] =
    as match {
      case Nil =>
        bs match {
          case Nil => Nil
          case _ => throw new IllegalArgumentException ("Lists must have the same length")
        }
      case ah :: at =>
        bs match {
          case Nil => throw new IllegalArgumentException ("Lists must have the same length")
          case bh :: bt =>
            f(ah,bh) :: zipWith(at, bt)(f)
        }
    }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith[A](sup: List[A], sub: List[A]): Boolean =
      (sub, sup) match {
        case (Nil,_) => true
        case (h :: t, Nil) => false
        case (h :: t, sh ::st) =>
          if (h != sh)
            false
          else
            startsWith(st, t)
      }

    (sub, sup) match {
      case (Nil, _) => true
      case (h :: t, Nil) => false
      case (h :: t, sh :: st) =>
        if (h != sh)
          hasSubsequence(st, sub)
        else
          if (startsWith(st, t))
            true
          else
            hasSubsequence(st, sub)
    }
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Chapter 3, tree functions
final class treeFunctions {

  // Exercise 3.25
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }


  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  // Exercise 3.28
  def map[A,B](tree: Tree[A])(f: A=>B): Tree[B] =
    tree match {
      case Leaf(value) => new Leaf[B](f(value))
      case Branch(left, right) => new Branch(map(left)(f), map(right)(f))
    }

  // Exercise 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) =>  g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((x,y) => x + y + 1)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)((x => x))((x,y) => x max y)

  // Exercise 3.27
  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 1)((x,y) => 1 + (x max y))

  def mapViaFold[A,B](tree: Tree[A])(f: A=>B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])((x,y) => Branch(x,y))
}
