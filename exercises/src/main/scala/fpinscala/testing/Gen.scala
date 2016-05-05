package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Status
case object Proven extends Status
case object Unfalsified extends Status
case object Exhausted extends Status



case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  import fpinscala.state._
  def &&(p: Prop): Prop = Prop {
    (m,n,rng) =>
      run(m,n,rng) match {
        case Right(_) => p.run(m,n,rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m,n,rng) =>
      run(m,n,rng) match {
        case Left(_) => p.run(m,n,rng)
        case x => x
      }
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)):Unit = {
    p.run(maxSize, testCases, rng) match {
      case Left(msg) => println("! test failed:\n" + msg)
      case Right((Unfalsified, n)) =>
        println("+ property unfalsified, ran " + n + " tests")
      case Right((Proven, n)) =>
        println("+ propterty proven, ran " + n + " tests")
      case Right((Exhausted, n)) =>
        println("+ property unfalsified up to max size, ran " + n + " tests")
    }
  }
}

object Prop {
  import fpinscala.state._
  import fpinscala.laziness._
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max: Int, n: Int, rng: RNG) => {
      def go(i: Int, j: Int, s: Stream[A], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s match {
          /*case Some(Some(h), t) =>
            try {
              if (f(h)) go(i+1, j, s, onEnd)
              else Left(h.toString)
            } catch {
              case e: Throwable =>
            }
          case Some(None, _) => Right((Unfalsified, i))
          case None => onEnd(i)*/
          case _ => onEnd(i)
        }
      go(0, n/3, gen.exhausitive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(gen)(rng)
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }

    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }
}

object Gen {
  import fpinscala.state._
  import fpinscala.laziness._
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), Stream(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), Stream(true, false))

  val uniform: Gen[Double] = Gen(State(RNG.double), Stream[Double]())

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)), Stream(start to stopExclusive: _*))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), Stream.constant(g.exhausitive.toList))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, stopExclusive).map(x => if (x % 2 == 0) x else x + 1)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, stopExclusive).map(x => if (x % 2 != 0) x else x + 1)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
    for {
      x <- choose(from, to)
      y <- if (x % 2 == 0) even(from, to) else odd(from, to)
    } yield (x, y)

  def listOfNViaMap2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(Gen.unit(List[A]()))(_.map2(_)(_ :: _))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(x => if (x) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A], Double)): Gen[A] = {
    val radio = (g1._2) / (g1._2 + g2._2)
    uniform.flatMap(x => if (x > radio) g2._1 else g1._1)
  }
}

case class Gen[+A](sample: State[RNG,A], exhausitive: Stream[A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhausitive.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample), exhausitive.flatMap(x => f(x).exhausitive))
  def map2[B,C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(b.sample)(f), Stream())

  def unsized: SGen[A] =
    SGen(_ => this)

  def listOfN(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(sample)), Stream.constant(exhausitive.toList))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(forSize andThen (_ map f))
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))
  def map2[B,C](b: SGen[B])(f: (A, B) => C):SGen[C] =
    SGen((n: Int) => forSize(n).map2(b.forSize(n))(f))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}
