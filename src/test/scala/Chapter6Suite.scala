package zone.slice.fpinscala

class Chapter6Suite extends munit.FunSuite {
  import chapter6._

  def createState(f: Int => Int): State[Int, Int] =
    State { state =>
      val n = f(state)
      (n, n)
    }

  val adder   = createState(_ + 1)
  val doubler = createState(_ * 2)

  test("6.10") {
    assertEquals(State.unit[String, Int](0).run("hello"), (0, "hello"))
    assertEquals(adder.map(_.toString).run(0), ("1", 1))
    val chained = adder.flatMap(_ => doubler)
    assertEquals(chained.run(0)._1, 2)
    assertEquals(
      State.sequence(List.fill(5)(adder)).run(1),
      (List(2, 3, 4, 5, 6), 6)
    )
    assertEquals(adder.map2(doubler)(_ + _).run(1), (6, 4))
  }

  test("p. 90") {
    def addOneProgram: State[Int, (Int, Int)] =
      for {
        o <- State.get
        _ <- State.modify[Int](_ + 1)
        v <- State.get
      } yield (o, v)
    assertEquals(addOneProgram.run(0), ((0, 1), 1))
  }

  val machine = Machine(true, coins = 10, candies = 5)

  test("6.11") {
    def run(inputs: List[Input]): (Int, Int) =
      Machine.simulateInputs(inputs).run(machine)._1
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    assertEquals(run(inputs), (14, 1))
    val inputs2 = List(Coin, Turn, Turn, Turn, Coin)
    assertEquals(run(inputs2), (12, 4))
  }
}
