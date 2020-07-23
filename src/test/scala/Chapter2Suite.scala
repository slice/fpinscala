package zone.slice.fpinscala

class Chapter2Suite extends munit.FunSuite {
  import chapter2._

  test("2.1") {
    assertEquals(Fib.fib(0), 0)
    assertEquals(Fib.fib(1), 1)
    assertEquals(Fib.fib(7), 13)
  }

  test("2.2") {
    assertEquals(IsSorted.isSorted(List(1, 2, 3, 4, 5, 6))(_ < _), true)
    assertEquals(IsSorted.isSorted(List(1, 2, 3, 6, 5, 4))(_ < _), false)
  }

  val add = (l: Int, r: Int) => l + r

  test("2.3") {
    assertEquals(Currying.curry(add)(1)(2), 3)
  }

  test("2.4") {
    assertEquals(Currying.uncurry(Currying.curry(add))(1, 2), 3)
  }

  test("2.5") {
    val f: Int => String = _.toString
    val g: String => Int = _.toInt
    assertEquals(Currying.compose(g, f)(5), 5)
  }
}
