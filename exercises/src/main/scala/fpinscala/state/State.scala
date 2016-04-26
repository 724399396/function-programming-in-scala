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
    val (value, nextRNG) = rng.nextInt
    (if (value < 0) -(value + 1) else value, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue + 1), next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(r: RNG, n: Int, acc: List[Int]): (List[Int], RNG) = {
      if (n <= 0)
        (acc, r)
      else {
        val (i, r1) = r.nextInt
        go(r1, n - 1, i :: acc)
      }
    }

    go(rng, count, List())
  }

  def positiveMax(n: Int): Rand[Int] =
    map(nonNegativeInt)(x => x / (Int.MaxValue + 1) * n)

  def double2: Rand[Double] =
    map(nonNegativeInt)(x => x / (Int.MaxValue + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng: RNG) => {
    val (a, r) = ra(rng)
    val (b, r2) = rb(r)
    (f(a, b), r2)
  }

  def intDouble2: Rand[(Int, Double)] =
    map2(int, double)((_,_))

  def doubleInt2: Rand[(Double, Int)] =
    map2(double, int)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))(map2(_,_)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (v, r) = f(rng)
    g(v)(r)
  }

  def positiveInt2: Rand[Int] =
    flatMap(int){ x =>
      if (x == Int.MinValue)
        positiveInt2
      else
        unit(x.abs)
    }

  def mapViaFlatMap[A,B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(x => unit(f(x)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] =
    flatMap(x => unit(f(x)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => {
        val (v, s2) = run(s)
        f(v).run((s2))
      }
    )

  def get: State[S, S] =
    State((s: S) => (s,s))

  def set(s: S): State[S, Unit] =
    State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    def help(input: Input): State[Machine, Unit] = {
      (machine: Matchine) => ((), (input, machine) match {
        case (_, Machine(_, 0, _) @ m) =>
          ((), m)
        case (Coin, Machine(true, candies, coins)) if candies > 0 =>
          ((), Machine(false, candies, coins + 1))
        case (Coin, Machine(locked, candies, coins)) =>
          ((), Machine(locked, candies, coins + 1))
        case (Turn, Machine(false, candies, coins)) =>
          ((), Machine(true, candies-1, conins))
        case (Turn, Machine(true, _, _) @ m) =>
          ((),m)
      }
    }


}
