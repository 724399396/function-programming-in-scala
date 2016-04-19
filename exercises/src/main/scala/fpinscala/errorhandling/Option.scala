package fpinscala.errorhandling


import java.util.regex.{Pattern, PatternSyntaxException}

import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(x) => x
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => b map (y => f(x, y)))

  def mkMatcher(pat: String): Option[Pattern] =
    try {
      Some(Pattern.compile(pat))
    } catch {
      case e: PatternSyntaxException => None
    }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat), mkMatcher(pat2))((a, b) => a.matcher(s).matches && b.matcher(s).matches)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List[B]()))((x, acc) => map2(f(x), acc)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}