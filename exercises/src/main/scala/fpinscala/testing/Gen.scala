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
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check = check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
}

trait SGen[+A] {

}

