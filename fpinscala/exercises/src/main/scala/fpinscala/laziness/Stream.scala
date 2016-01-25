package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def go(left: Stream[A], acc: List[A]): List[A] = left match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = this match {
    case Cons(t,h) => Some(t())
    case Empty => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a),b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,_),1) => Some((h(), (empty, 0)))
      case (Cons(h,t),n) if n == 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B,C](other: Stream[B])(f: (A,B) => C) : Stream[C] =
    unfold((this,other)) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()),(t1(),t2()))
    }

  def zipViaUnfold[B](other: Stream[B]): Stream[(A,B)] =
    zipWithViaUnfold(other)((_,_))

  def zipAllWithUnfold[B,C](other: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold((this,other)) {
      case (Empty, Empty) => None
      case (Cons(h,t), Empty) => Some(f(Some(h()),None), (t(),Empty))
      case (Empty, Cons(h,t)) => Some(f(None, Some(h())), (Empty, t()))
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(Some(h1()), Some(h2())), (t1(),t2()))
    }

  def zipAllViaUnfold[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    zipAllWithUnfold(other)((_,_))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAllViaUnfold(s).takeWhile(!_._2.isEmpty).forAll{ case (h1,h2) => h1 == h2}

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z,Stream(z))) { (a, p0) =>
      lazy val p1 = p0
      val n = f(a, p1._1)
      (n, cons(n,p1._2))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  val fibsViaUnfold =
    unfold((0,1)){ case (f0,f1) => Some(f0, (f1,f0+f1))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s,s+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1,1))

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails.exists(_ startsWith s2)
}