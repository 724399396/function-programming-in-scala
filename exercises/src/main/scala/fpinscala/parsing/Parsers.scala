package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char[A](c: Char): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
  def slice[A](a: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  def many1[A](p: Parser[A]): Parser[List[A]]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def label[A](e: String)(p: Parser[A]): Parser[A]
  def scope[A](e: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: => Parser[A]): Parser[A]

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p,p2))(f.tupled)
  def many1ViaMap2Many[A](p: => Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)
  def manyViaOrMap2Succeed[A](p: Parser[A]): Parser[List[A]] =
    map2(p, manyViaOrMap2Succeed(p))(_ :: _) or succeed(List[A]())
  def listOfNViaMap2Succeed[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List[A]())
    else map2(p, listOfNViaMap2Succeed(n-1,p))(_ :: _)

  def productViaFlatMap[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))
  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a,b)))
  def mapViaFlatMap[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))


  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: scala.util.matching.Regex): Parser[String]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: =>Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    import fpinscala.testing._
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]) =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]) =
      equal(p, p.map(identity))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object Instance {
  type Parser[+A] = String => Either[ParseError, A]

  object ParsersImpl extends Parsers[Parser] {
    def string(s: String): Parser[String] =
      (input: String) =>
        if (input.startsWith(s))
          Right(s)
        else
          Left(Location(input, 0).toError("Expected: " + s))

    def regex(r: Regex): Parser[String] =
      (input: String) =>
        r.findFirstIn(input) match {
          case Some(x) => Right(x)
          case None => Left(Location(input,0).toError("Regex not match, Expected: " + r.toString()))
        }

    // so inner classes may call methods of trait
    override def run[A](p: _root_.fpinscala.parsing.Instance.Parser[A])(input: String): Either[_root_.fpinscala.parsing.ParseError, A] = ???

    override def char[A](c: Char): _root_.fpinscala.parsing.Instance.Parser[A] = ???

    override def or[A](p1: _root_.fpinscala.parsing.Instance.Parser[A], p2: => _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[A] = ???

    override def listOfN[A](n: Int, p: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[List[A]] = ???

    override def many[A](p: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[List[A]] = ???

    override def map[A, B](p: _root_.fpinscala.parsing.Instance.Parser[A])(f: (A) => B): _root_.fpinscala.parsing.Instance.Parser[B] = ???

    override def slice[A](a: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[String] = ???

    override def product[A, B](p: _root_.fpinscala.parsing.Instance.Parser[A], p2: => _root_.fpinscala.parsing.Instance.Parser[B]): _root_.fpinscala.parsing.Instance.Parser[(A, B)] = ???

    override def many1[A](p: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[List[A]] = ???

    override def flatMap[A, B](p: _root_.fpinscala.parsing.Instance.Parser[A])(f: (A) => _root_.fpinscala.parsing.Instance.Parser[B]): _root_.fpinscala.parsing.Instance.Parser[B] = ???

    override def label[A](e: String)(p: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[A] = ???

    override def scope[A](e: String)(p: _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[A] = ???

    override def attempt[A](p: => _root_.fpinscala.parsing.Instance.Parser[A]): _root_.fpinscala.parsing.Instance.Parser[A] = ???


  }

}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JBoolean(get: Boolean) extends JSON
  case class JString(get: String) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String,JSON]) extends JSON




  def parse[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    def parseNull: Parser[JSON] =
      string("null").map(_ => JNull)
    def parseNumber =
      "\\d+\\.?\\d+".r.map(x => JNumber(x.toDouble))
    def parseBool =
      or("true", "false").map(x => JBoolean(if (x == "true") true else false))
    def parseString =
      ("\"" ** "[^\"]+".r ** "\"").map {
        case ((_,x), _) => JString(x)
      }
    def parseArray =
      ("[" ** many(parse(P) ** ",") ** "]").map {
        case ((_, x), _) => JArray(x.toIndexedSeq.map(_._1))
      }
    def parseObject =
      ("{" ** many(parseString ** ":" ** parse(P)) ** "}").map {
        case ((_, x),_) => x.map {
          case ((k, _), v) => (k.get,v)
        }.toMap
      }.map(x => JObject(x))
    parseNull or parseNumber or parseBool or parseString or parseArray or parseObject
  }

}