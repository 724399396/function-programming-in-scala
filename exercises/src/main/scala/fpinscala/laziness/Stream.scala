package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
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
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((x,acc) => if (p(x)) cons(x,acc) else empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((x,_) => Some(x))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a,acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a,acc) => if (f(a)) cons(a,acc) else acc)

  def append[B>:A](b: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](b)((a,acc) => cons(a,acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((a,acc) => f(a) append acc)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold[B, Stream[A]](this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h, t),0) => Some(h(), (empty, 0))
      case (Cons(h, t),n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,other)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipWithAll[B,C](other: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold(this, other) {
      case (Empty, Empty) => None
      case (Cons(h,t), Empty) => Some((f(Some(h()), None), (t(),Empty)))
      case (Empty, Cons(h,t)) => Some((f(None, Some(h())), (Empty,t())))
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(Some(h1()), Some(h2())), (t1(),t2())))
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(other)((_,_))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s@Cons(_,t) => Some((s, t()))
      case Empty => None
    } append Stream(empty)

  // not efficent
  def scanRight_1[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))

  def scanRight_2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, cons(z,empty[B]))) {
      case (x, (sum, acc)) =>
        val b = f(x,sum)
        (b, cons(b, acc))
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a+b))
    }
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0,1)){ case (a,b) => Some((a, (b,a+b)))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x+1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def onesViaUnFold: Stream[Int] =
    unfold(())(_ => Some(1, ()))
}