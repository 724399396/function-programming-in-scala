package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] =  new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(ao1: Option[A], ao2: Option[A]) = ao1 orElse ao2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = identity
  }

  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(a1: A, a2: A) = m.op(a2,a1)
    def zero = m.zero
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x,y,z)) {
      case (x,y,z) =>
        m.op(m.op(x,y), z) == m.op(x,m.op(y,z))
    } && Prop.forAll(gen)(x => m.op(m.zero, x) == x)

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b,a) =>m.op(b,f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))((a:A)=> (b:B)=> f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as(0))
      case _ => as.splitAt(as.length / 2) match {
        case (l,r) => m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
      }
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val orderMonoid = new Monoid[Option[(Int,Int,Boolean)]] {
      def op(o1: Option[(Int,Int,Boolean)], o2: Option[(Int,Int,Boolean)]) =
        (o1,o2) match {
          case (Some((x1,y1,p)), Some((x2,y2,q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }

    foldMapV(ints, orderMonoid)(i => Some((i,i,true))).map(_._3).getOrElse(true)

  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1,a2)(m.op)
    def zero = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM = par(m)
    v.length match {
      case 0 => parM.zero
      case 1 => Par.lazyUnit(f(v(0)))
      case _ => v.splitAt(v.length / 2) match {
        case (l, r) => parM.op(parFoldMap(l, m)(f), parFoldMap(r, m)(f))
      }
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(w1: WC, w2: WC): WC = (w1,w2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1) ,r2)
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Part(l, w, r), Stub(s)) => Part(l,w,r+s)
      case (Stub(s), Part(l,w,r)) => Part(s+l,w,r)
    }
    val zero = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char) =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(s, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l,w,r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B =
      (a) => B.op(f1(a), f2(a))
    def zero = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K,V]] {
    def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) {
        (acc,k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }

    override def zero: Map[K, V] = Map[K,V]()
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A,Int](intAddition))((a: A) => Map(a->1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(x) => f(x)
      case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(x) => f(z,x)
      case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(x) => f(x,z)
      case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(x) => f(x)
      case None => mb.zero
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case Some(x) => f(z,x)
      case None => z
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case Some(x) => f(x,z)
      case None => z
    }
}

