package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }
    go(this, Nil).reverse
  }

  def toListFast: List[A] = {
    val buf = new scala.collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = {
      s match {
        case Empty => buf.toList
        case Cons(h, t) =>
          buf += h()
          go(t())
      }
    }
    go(this)
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

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, acc) => p(h) && acc)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else acc)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((h, acc) => cons(h, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h).append(acc))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this){
      case Cons(h, t) if n > 1 => Some((h(), t()))
      case Cons(h, _) if n == 1 => Some((h(), empty))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B,C](bb: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, bb)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](bb: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(bb)((_,_))

  def zipWithAll[B,C](bb: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] =
    unfold((this, bb)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((f(Some(h()), None: Option[B]), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some((f(None: Option[A], Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{
      case (a,b) => a == b
    }

  def tails: Stream[Stream[A]] =
    unfold[Stream[A],Stream[A]](this) {
      case Empty => None
      case s => Some((this, s drop 1))
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) {
      (x, p) =>
        val res = f(x, p._1)
        (res, cons(res, p._2))
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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def help(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, help(b, a + b))
    }
    help(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some((x, x)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x+1)))

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)){case (a, b) => Some((a, (b, a + b)))}

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (_.startsWith(s2))
}