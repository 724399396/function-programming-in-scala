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

trait Prop {
  def check: Either[FailedCase, SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))
  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State(s => (a,s)), bounded(Stream.constant(a)))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), bounded(Stream(true, false)))
  def listOfN[A](a: Gen[A], n: Int): Gen[List[A]] = Gen(State.sequence(List.fill(n)(a.sample)), Stream())
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def forall[A](a: Gen[A])(f: A => Boolean): Prop = ???
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)), bounded(Stream(start until stopExclusive: _*)))
  def choose(start: Double, stopExclusive: Double): Gen[Double] = Gen(State(RNG.double).map(x => start + x % (stopExclusive - start)), unbounded)

  def map2Stream[A,B,C](s1: Stream[A], s2: => Stream[B])(f: (A,=>B) => C): Stream[C] =
     for(a <- s1; b <- s2) yield { f(a,b) }

  def map2Option[A,B,C](o1: Option[A], o2: Option[B])(f: (A,B) => C): Option[C]=
     for(a <- o1; b <- o2) yield { f(a,b) }

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 ==0) stopExclusive - 1 else stopExclusive)
        .map(n => if(n % 2 == 0) n else n + 1)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive)
        .map(n => if (n % 2 != 0) n else n + 1)

  def sameParity(start: Int, stopExclusive: Int): Gen[(Int,Int)] =
    for {
      i <- choose(start, stopExclusive)
      j <- if (i % 2 == 0) even(start,stopExclusive) else odd(start, stopExclusive)
    } yield (i,j)

  def listOfN_1[A](n: Int, a: Gen[A]): Gen[List[A]] =
    List.fill(n)(a).foldRight(unit(List[A]()))((x,acc) => x.map2(acc)(_ :: _))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(State(RNG.boolean).flatMap(b => if (b) g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive))

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    s1.zipAllViaUnfold(s2).flatMap{case (a1,a2) => Stream(a1.toList ++ a2.toList: _*)}

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val total = g1._2 + g2._2
    choose(g1._2, total).flatMap(x => if (x < g1._2) g1._1 else g2._1)
  }
}

case class Gen[A](sample: State[RNG,A], exhaustive: Domain[A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f),
    map2Stream(exhaustive, g.exhaustive)(map2Option(_,_)(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample),
    exhaustive.flatMap{
      case None => unbounded
      case Some(x) => f(x).exhaustive
  })
}

trait SGen[+A] {

}

