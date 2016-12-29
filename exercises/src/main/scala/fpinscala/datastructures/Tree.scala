package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    tree match {
      case Leaf(x) => f(x)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sumViaFold(tree: Tree[Int]) =
    fold(tree)(identity)(_ + _)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)(1 + _.max(_))

  def mapViaFold[A,B](tree: Tree[A])(f: A => B) =
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}