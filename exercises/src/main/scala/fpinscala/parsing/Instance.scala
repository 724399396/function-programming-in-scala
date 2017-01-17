package fpinscala.parsing

import scala.util.matching.Regex
import InstanceTypes._

object InstanceTypes {
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }

    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _  => this
    }

    def addCommit(isCommited: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommited)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e,c) => Failure(f(e), c)
      case _ => this
    }

    def advanceSuccess(n: Int) = this match {
      case Success(a,m) => Success(a,m+n)
      case _ => this
    }
  }

  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object ParsersImpl extends Parsers[Parser] {
  def run[A](p: Parser[A])(s: String): Either[ParseError, A] = {
    val s0 = ParseState(Location(s))
    p(s0).extract
  }

  def succeed[A](a: A): Parser[A] = _ => Success(a,0)

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case Failure(e, false) => p2(s)
      case r => r
    }

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a,n) => f(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case e@Failure(_,_) => e
    }

  def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    s => {
      val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
      if (i == -1)
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), i != 0)
    }
  }

  def regex(r: Regex): Parser[String] = {
    val msg ="regex " + r
    s => r.findFirstIn(s.input) match {
      case None => Failure(s.loc.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc,msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def fail[A](msg: String): Parser[A] =
    s => Failure(s.loc.toError(msg), true)

  def attempt[A](p: => Parser[A]): Parser[A] =
    s => p(s).uncommit

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_,n) => Success(s.slice(n),n)
      case f@Failure(_,_) => f
    }

  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p (s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p,offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }
}