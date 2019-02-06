package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(h,xs) => h + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h,xs) => h * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(h, Cons(2, Cons(4, _))) => h
    case Nil => 42
    case Cons(h, Cons(y, Cons(3, Cons(4, _)))) => h + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, xs) => f(h, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can't get tail of list. Reason: Given list is empty.")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Can't setHead on empty list!")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, xs) if f(h) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can't get init of empty list!")
    case Cons(_, Nil) => Nil
    case Cons(h, xs) => Cons(h, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def sumFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 1)(_ * _)

  def lengthFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)((acc, _) => acc + 1)

  def reverseList[A](ns: List[A]): List[A] = foldLeft(ns, List[A]())((acc, h) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addListElementsPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addListElementsPairwise(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith[A, B, C](t1, t2)(f))
  }
}
