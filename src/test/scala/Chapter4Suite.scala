package zone.slice.fpinscala

class Chapter4Suite extends munit.FunSuite {
  import chapter4._

  val five = Some(5)
  val six = Some(6)
  val none: Option[Int] = None

  test("4.1") {
    assertEquals(five.map(_ + 1), Some(6))
    assertEquals(none.map(_ + 1), none)
    assertEquals(five.flatMap(v => Some(v + 2)), Some(7))
    assertEquals(five.flatMap(_ => none), none)
    assertEquals(five.getOrElse(2), 5)
    assertEquals(none.getOrElse(2), 2)
    assertEquals(five.orElse(six), Some(5))
    assertEquals(none.orElse(six), Some(6))
    assertEquals(five.filter(_ == 6), none)
    assertEquals(five.filter(_ == 5), five)
  }

  test("4.2") {
    assertEquals(OptionAux.variance(List(1.0, 2.0)), Some(0.25))
  }

  test("4.3") {
    assertEquals(Option.map2(five, five)(_ + _), Some(10))
    assertEquals(Option.map2B(five, five)(_ + _), Some(10))
  }

  test("4.4") {
    assertEquals(Option.sequence(somes), Some(List(5, 2, 3)))
    assertEquals(Option.sequence(partial), None)
  }

  val somes = List(Some(5), Some(2), Some(3))
  val partial = List(None, Some(1), None)

  test("4.5") {
    import OptionAux.Try

    assertEquals(Option.sequence2(somes), Option.sequence(somes))
    assertEquals(Option.sequence2(partial), Option.sequence(partial))
    assertEquals(Option.traverse(List("1", "2", "no"))(n => Try(n.toInt)), None)
    assertEquals(Option.traverse(List("1", "2"))(n => Try(n.toInt)), Some(List(1, 2)))
  }

  val success: Either[String, Int] = Right(5)
  val error: Either[String, Int] = Left("failed!")

  test("4.6") {
    assertEquals(success.map(_ + 1), Right(6))
    assertEquals(success.flatMap(five => error), error)
    assertEquals(for {
      five <- success
      someInt <- error
    } yield someInt, error)
    assertEquals(error.orElse(success), success)
    assertEquals(success.map2(error)(_ + _), error)
  }

  test("4.7") {
    assertEquals(Either.sequence(List(success, success)), Right(List(5, 5)))
    assertEquals(Either.sequence(List(success, success, error)), Left("failed!"))
  }
}
