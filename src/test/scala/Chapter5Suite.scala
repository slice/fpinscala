package zone.slice.fpinscala

class Chapter5Suite extends munit.FunSuite {
  import chapter5._
  val stream = Stream(1, 2, 3)

  test("5.1") {
    assertEquals(stream.toList, List(1, 2, 3))
  }

  test("5.2") {
    assertEquals(stream.take(2).toList, List(1, 2))
    assertEquals(stream.drop(2).toList, List(3))
  }

  test("5.3") {
    assertEquals(stream.takeWhile(_ < 3).toList, List(1, 2))
  }
}
