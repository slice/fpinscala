package zone.slice.fpinscala

class Chapter8Suite extends munit.FunSuite {
  import chapter8._
  import chapter6.SimpleRNG

  // val goodProp = new Prop {
  //   val check = true
  // }

  // val badProp = new Prop {
  //   val check = false
  // }

  // test("8.3") {
  //   assertEquals((goodProp && goodProp).check, true)
  //   assertEquals((goodProp && badProp).check, false)
  // }

  val rng = SimpleRNG(5)

  def run[A](gen: Gen[A]): A =
    gen.sample.run(rng)._1

  test("8.4") {
    val num = run(Gen.choose(1, 4))
    assertEquals(num >= 1 && num < 4, true)
  }

  test("8.5") {
    assertEquals(run(Gen.pure(1)), 1)
    run(Gen.boolean)
    val booleans = run(Gen.listOfN(10, Gen.boolean))
    assertEquals(booleans.size, 10)
  }

  test("8.6") {
    val gen    = Gen.boolean.flatMap(b => Gen.pure(s"hello, $b!"))
    val result = run(gen)
    assertEquals(result == "hello, true!" || result == "hello, false!", true)
    run(gen.listOfN(Gen.choose(1, 11)))
  }

  test("8.7") {
    val one    = Gen.pure(1)
    val two    = Gen.pure(2)
    val result = run(Gen.union(one, two))
    assertEquals(result == 1 || result == 2, true)
  }

  def check(p: Prop): Result =
    Prop.check(p, 100, 100, rng)
  def assertPasses(p: Prop): Unit =
    assertEquals(check(p).passed, true)
  def assertFails(p: Prop): Unit =
    assertEquals(check(p).failed, true)

  test("8.9") {
    import Prop.forAll
    import Gen._

    // `list.reverse.reverse` is the same as `list`
    val reverseTwiceIdentity = forAll(listOf(int))(l => l.reverse.reverse == l)
    assertPasses(reverseTwiceIdentity)

    // all integers are positive (hey, not true!)
    val allIntsPositive = forAll(int)(_ > 0)
    assertFails(allIntsPositive)

    // `list.sum` is the same as `list.foldLeft(0)(_ + _)`
    val sumProp = forAll(listOf(int))(l => l.sum == l.foldLeft(0)(_ + _))
    assertPasses(reverseTwiceIdentity && sumProp)
    assertFails(reverseTwiceIdentity && allIntsPositive)

    // all elements in a list (of at least size 1) of ints within [-10, 11) are
    // smaller than the maximum
    val maxProp = forAll(listOf1(choose(-10, 11))) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    assertPasses(maxProp)
  }

  def isSorted(ints: List[Int]) = {
    if (ints.size == 1) true // O_O.
    else {
      (ints, ints.tail).zipped.map(_ <= _).forall(identity)
    }
  }

  // Exercise 8.14
  test("8.14") {
    import Prop.forAll
    import Gen._

    val sortedWorks = forAll(listOf1(int))(ints => isSorted(ints.sorted))
    assertEquals(check(sortedWorks).passed, true)
  }

  test("laws") {
    import chapter7._
    import java.util.concurrent.{ExecutorService, Executors}
    val es: ExecutorService = Executors.newCachedThreadPool()

    val in   = Par.map(Par.pure(1))(_ + 1)
    val out  = Par.pure(2)
    val prop = Prop.check(Par.run(es)(in) == Par.run(es)(out))

    assertEquals(check(prop).passed, true)
  }
}
