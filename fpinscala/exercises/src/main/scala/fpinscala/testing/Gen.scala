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
  def unit[A](a: => A): Gen[A] = ???
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def forall[A](a: Gen[A])(f: A => Boolean): Prop = ???
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(x => start + x % (stopExclusive - start)))
}

case class Gen[A](smaple: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

