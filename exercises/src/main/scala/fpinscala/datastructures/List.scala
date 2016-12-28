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
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
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
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => acc+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumViaFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]) = foldLeft(l, 0)((acc,_) =>acc+1)

  def reverseViaFoldLeft[A](l: List[A]) = foldLeft(l, Nil: List[A])((acc,x) => Cons(x, acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B) =
    foldLeft(l, (x: B) => x)((g, x) => (n: B) => g(f(x,n)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B) =
    foldRight(l, (x: B) => x)((x, g) => (n: B) => g(f(n,x)))(z)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x,acc) => Cons(x,acc))

  def concat[A](l: List[List[A]]) =
    foldRight(l, Nil: List[A])(append)

  def addOne(l: List[Int]) = foldRight(l, Nil: List[Int])((x,acc) => Cons(x+1,acc))

  def double2String(l: List[Double]) = foldRight(l, Nil: List[String])((x,acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((x, acc) => if (f(x)) Cons(x,acc) else acc)

  def filterOdd(l: List[Int]): List[Int] =
    filter(l)(_ % 2 == 0)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def addList(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addList(t1,t2))
      case _ => Nil
    }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
    (l1, l2) match {
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
      case _ => Nil
    }
}
