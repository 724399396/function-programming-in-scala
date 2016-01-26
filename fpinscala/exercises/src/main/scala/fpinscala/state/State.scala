package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res,next) = rng.nextInt
    (if (res == Int.MinValue ) 0 else res.abs, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, n) = nonNegativeInt(rng)
    ((i / (1 + Int.MaxValue)).toDouble, n)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d,r1) = double(rng)
    val (i,r2) = r1.nextInt
    ((d,i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (i,r) = rng.nextInt
      val (list,r2) = ints(count - 1)(r)
      (i :: list, r2)
    }
  }

  def positiveMax(n: Int): Rand[Int] = map(int)(a => ((a / (Int.MaxValue + 1).toDouble) * n).toInt)

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (1 + Int.MaxValue))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (res1, r1) = ra(rng)
      val (res2, r2) = rb(r1)
      (f(res1,res2), r2)
    }

  def intDoubleViaMap2 = map2(int, double)((_,_))

  def doubleIntViaMap2 = map2(double, int)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](List()))((x,acc) => map2(x,acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a,r) = f(rng)
      g(a)(r)
    }

  def positiveIntViaFlatMap(n: Int): Rand[Int] = flatMap(int){
    x => if (x == Int.MinValue) positiveIntViaFlatMap(n) else unit(x)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] =
    flatMap{a => unit(f(a))}
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (r,s1) = run(s)
      f(r).run(s1)
    })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequenceViaFoldRight[S, A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S, List[A]](List())){(x,acc) => x.map2(acc)(_ :: _)}

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    def go(s: S, as: List[State[S, A]], acc: List[A]): (List[A], S) =
      as match {
        case Nil => (acc.reverse, s)
        case t :: h =>
          val (n,s1) = t.run(s)
          go(s1, h, n :: acc)
      }
    State(s => go(s,fs, List[A]()))
  }

  def get[S]: State[S,S] = State (
    s => (s,s)
  )

  def set[S](newState: S): State[S,Unit] = State (
    _ => ((), newState)
  )

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (Coin, Machine(false, _, _)) => s
        case ((Turn, Machine(true, _, _))) => s
        case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
        case (_, Machine(_, 0, _)) => s
      }

    for {
      _ <- sequence(inputs map (modify[Machine] compose update))
      s <- get
    } yield (s.candies, s.coins)
  }
}
