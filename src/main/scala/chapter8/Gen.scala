package zone.slice.fpinscala
package chapter8

import chapter6.{State, RNG, SimpleRNG}

// *eyeroll*
object Types {
  type FailedCase   = String
  type SuccessCount = Int
  type TestCases    = Int
  type MaxSize      = Int
}

import Types._

sealed trait Result {
  def failed: Boolean
  def passed: Boolean = !failed
}
case object Passed extends Result {
  var failed = false
}
case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  var failed = true
}
case object Proved extends Result {
  val failed = false
}

/**
  * A generator of arbitrary `A` values.
  */
case class Gen[+A](sample: State[RNG, A]) {
  // oops, took a look at solutions and it turns out we can just use state.. lol
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  // Gen(State { state =>
  //   val (a, rng2) = sample.run(state)
  //   (f(a), rng2)
  // })

  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))
  // Gen(State { state =>
  //   val (a, rng2) = sample.run(state)
  //   val (b, rng3) = f(a).sample.run(rng2)
  //   (b, rng3)
  // })

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  // Exercise 8.10
  /**
    * Converts this `Gen` to an unsized `SGen`.
    */
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 8.11
  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n) flatMap { f(_).forSize(n) })
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // Exercise 8.3
  // def &&(p: Prop): Prop = {
  //   val result = this.check && p.check
  //   new Prop {
  //     val check: Boolean = result
  //   }
  // }

  // Exercise 8.9
  def &&(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        // immediately fail (short circuiting)
        case f: Failed => f
        case _         => p.run(max, n, rng)
      }
    }

  def ||(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case f: Failed => p.run(max, n, rng)
        // immediately pass (short circuiting)
        case o => o
      }
    }
}

object Prop {
  // dangerous side effects!!! >:O
  def check(
      prop: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Result = {
    val result = prop.run(maxSize, testCases, rng)
    result match {
      case Failed(msg, n) => println(s"! Failed after $n passed tests: $msg")
      case Passed         => println(s"+ OK, passed $testCases tests.")
      case Proved         => println(s"+ OK, property proved.")
    }
    result
  }

  def check(p: => Boolean): Prop =
    Prop { (_, _, _) =>
      if (p) Proved else Failed("()", 0)
    }

  /**
    * Build a `Prop` from a `Gen` by asserting that all possibly generated
    * values satisify a condition.
    */
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Failed(a.toString, i)
            } catch {
              case e: Exception => Failed(buildFailureMessage(a, e), i)
            }
        }
        .find(_.failed)
        .getOrElse(Passed)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(n => g.forSize(n))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      // "For each size, generate this many random cases."
      val casesPerSize = (n + (max - 1)) / max
      // "Make one property per size, but no more than `n` properties."
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      // "Combine them all into one property."
      val prop: Prop =
        props
          .map(p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
            }
          )
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
    }

  private def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))
  private def buildFailureMessage[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object Gen {
  // numbers are cool, right?
  def int: Gen[Int] =
    Gen(State(RNG.int))
  def posInt: Gen[Int] =
    int.map(_.abs)

  // Exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State { rng =>
      // basic math time?? thanks soren
      val (i, rng2) = rng.nextInt
      val range     = stopExclusive - start
      val offset    = ((i % range) + range) % range
      val result    = start + offset
      (result, rng2)
    })

  // ... i am, once again, implementing `traverse`. ＼（´Ｏ｀）／
  def traverse[A, B](as: List[Gen[A]])(f: Gen[A] => Gen[B]): Gen[List[A]] =
    as match {
      case Nil    => pure(Nil)
      case h :: t => h.flatMap(gen => traverse(t)(f).map(gen :: _))
    }
  def sequence[A](as: List[Gen[A]]): Gen[List[A]] =
    traverse(as)(identity)
  // okay, well, turns out we didn't even need these. but i'm keeping 'em
  // anyways because it forced me to implement `flatMap` >:c

  // Exercise 8.5
  def pure[A](a: => A): Gen[A] =
    Gen(State.unit(a)) // i don't like calling this `unit` so
  def boolean: Gen[Boolean] =
    choose(0, 2).map(_ == 1) // bit bad, but eh.
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  // def listOf[A](a: Gen[A]): Gen[List[A]] = {
  //   // a bit arbitrary, but what else? hmm. :THINKING:
  //   choose(1, 101).flatMap(n => { a.listOfN(Gen.pure(n)) })
  // }

  // Exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(g.listOfN(_))

  // Exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(1 max n))
}
