package zone.slice.fpinscala

import java.util.concurrent.Executors

class Chapter7Suite extends munit.FunSuite {
  import chapter7._

  val seq = Vector.range(1, 101)
  val sum = seq.sum

  test("plainSum") {
    assertEquals(ParUse.plainSum(seq), sum)
  }

  test("divideAndConquerSum") {
    assertEquals(ParUse.divideAndConquerSum(seq), sum)
  }

  val service = Executors.newCachedThreadPool()

  test("parSum") {
    assertEquals(Par.run(service)(ParUse.parSum(seq)), sum)
  }

  test("7.1") {
    val par = Par.map2(Par.lazyUnit(1), Par.lazyUnit(2))(_ + _)
    assertEquals(Par.run(service)(par), 3)
  }

  test("7.4") {
    val addFive: Int => Int = _ + 5
    val addFiveAsync        = Par.asyncF(addFive)
    assertEquals(Par.run(service)(addFiveAsync(1)), 6)
  }

  test("7.5") {
    val pas = List.range(1, 6).map(Par.lazyUnit(_))
    assertEquals(Par.run(service)(Par.sequence(pas)), List(1, 2, 3, 4, 5))
  }

  test("7.6") {
    val pa = Par.parFilter(List.range(1, 6))(_ > 3)
    assertEquals(Par.run(service)(pa), List(4, 5))
  }
}
