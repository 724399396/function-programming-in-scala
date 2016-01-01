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
    case Nil => sys.error("wrong")
    case Cons(_, x) => x
  }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => sys.error("setHead on Empty list")
      case Cons(_,t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("init on Empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
      

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(l: List[Double]) = foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]) = foldLeft(l, 0) ((acc,_) => acc + 1)

  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((xs,x) => Cons(x, xs))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B):B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B):B =
    foldLeft(l, (b: B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b: B) => b)((a,g) => b => g(f(b,a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((x,xs) => Cons(x,xs))

  def concatenates[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(appendViaFoldRight)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a,acc) => Cons(a+1,acc))

  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a,r) => Cons(a.toString, r))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B])((a,r) => Cons(f(a), r))

  def map_1[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((a,r) => Cons(f(a), r))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x,xs) => if(f(x)) Cons(x,xs) else xs)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((x,xs) => appendViaFoldRight(f(x), xs))

  def flatMap_1[A,B](l: List[A])(f: A => List[B]): List[B] =
    concatenates(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t), Cons(h2,t2)) if h == h2 => startsWith(t,t2)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
