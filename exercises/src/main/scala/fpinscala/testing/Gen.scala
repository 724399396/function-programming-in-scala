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

case class Prop (run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max,n,rng) =>
      run(max,n,rng) match {
        case Proved => p.run(max,n,rng)
        case Passed => p.run(max,n,rng)
        case f@Falsified(_,_) => f
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max,n,rng) =>
      run(max,n,rng) match {
        case Passed => Passed
        case Proved => Proved
        case Falsified(msg,_) => p.tag(msg).run(max,n,rng)
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max,n,rng) match {
      case Passed => Passed
      case Proved => Proved
      case Falsified(e, i) => Falsified(msg + "\n" + e, i)
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(gen)(rng).zipWith(Stream.from(0))((_,_)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s" + OK< passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop{ (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(x => if (x) g1 else g2)

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (ga1, gw1) = g1
    val (ga2, gw2) = g2
    double.flatMap(x => if (((gw1+gw2) * x) > gw1) ga2 else ga1)
  }
}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(x => forSize(x).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(x => forSize(x).flatMap(a => f(a).forSize(x)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(size => g.listOfN(size))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

}

