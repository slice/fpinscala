package zone.slice.fpinscala
package chapter8

import chapter6.{State, RNG}

// *eyeroll*
object Types {
  type FailedCase   = String
  type SuccessCount = Int
  type TestCases    = Int
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

/**
  * A generator of arbitrary `A` values.
  */
case class Gen[A](sample: State[RNG, A]) {
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
}

case class Prop(run: (TestCases, RNG) => Result) {
  // Exercise 8.3
  // def &&(p: Prop): Prop = {
  //   val result = this.check && p.check
  //   new Prop {
  //     val check: Boolean = result
  //   }
  // }

  // Exercise 8.9
  def &&(p: Prop): Prop =
    Prop { (n, rng) =>
      run(n, rng) match {
        // immediately fail (short circuiting)
        case f: Failed       => f
        case ps: Passed.type => p.run(n, rng)
      }
    }

  def ||(p: Prop): Prop =
    Prop { (n, rng) =>
      run(n, rng) match {
        case f: Failed => p.run(n, rng)
        // immediately pass (short circuiting)
        case ps: Passed.type =>
          ps
      }
    }
}

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = {
    // a bit arbitrary, but what else? hmm. :THINKING:
    choose(1, 101).flatMap(n => { a.listOfN(Gen.pure(n)) })
  }

  /**
    * Build a `Prop` from a `Gen` by asserting that all possibly generated
    * values satisify a condition.
    */
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
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

  private def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))
  private def buildFailureMessage[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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
    choose(0, 2).map(_ % 2 == 0) // bit bad, but eh.
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)
}
