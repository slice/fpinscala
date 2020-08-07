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

  test("works") {
    val listOfInts = Gen.listOf(Gen.int)
    val prop       = Gen.forAll(listOfInts)(l => l.reverse.reverse == l)
    assertEquals(prop.run(1000, rng).passed, true)

    val fails  = Gen.forAll(Gen.int)(_ > 0)
    val result = fails.run(1000, rng)
    assertEquals(result.failed, true)

    val alsoPasses = Gen.forAll(listOfInts)(l => l.sum == l.foldLeft(0)(_ + _))
    assertEquals((prop && alsoPasses).run(1000, rng).passed, true)
    assertEquals((prop && fails).run(1000, rng).failed, true)
  }
}
