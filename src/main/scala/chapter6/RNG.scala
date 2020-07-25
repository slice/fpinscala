package zone.slice.fpinscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  private def restrict(
      n: Int,
      forbidden: Int = Int.MinValue,
      modifier: Int = 1
  ): Int =
    if (n == forbidden) n + modifier else n

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    val nn     = math.abs(restrict(n) % Int.MaxValue)
    (nn, r)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = rng.nextInt
    val nn     = restrict(n, Int.MaxValue, -1).toDouble / Int.MaxValue
    (nn, r)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((n, d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (n, r2) = r1.nextInt
    ((d, n), r2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r1)  = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (n, r)  = rng.nextInt
      val (t, r2) = ints(count - 1)(r)
      (n :: t, r2)
    }
  }

  // whoo, finally. (p. 84)
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  def double2: Rand[Double] =
    map[Int, Double](int) { int =>
      restrict(int, Int.MaxValue, -1).toDouble / Int.MaxValue
    }

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDouble2: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt2: Rand[(Double, Int)] =
    both(double, int)

  // Exercise 6.8
  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = f(a)(r1)
      (b, r2)
    }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil    => unit(Nil)
      case h :: t => flatMap(h)(a => map(sequence(t))(a :: _))
    }
  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod       = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // Exercise 6.9
  def map_2[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))
  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n       = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
