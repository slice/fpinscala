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

  test("5.4") {
    assertEquals(stream.forAll(_ < 5), true)
    assertEquals(stream.forAll(_ < 3), false)
  }

  test("5.5") {
    assertEquals(stream.takeWhile2(_ < 3).toList, List(1, 2))
  }

  test("5.6") {
    assertEquals(stream.headOption2, Some(1))
    assertEquals(Stream.empty.headOption2, None)
  }

  test("5.7") {
    assertEquals(stream.map(_ + 1).toList, List(2, 3, 4))
    assertEquals(stream.filter(_ < 3).toList, List(1, 2))
    assertEquals(stream.append(Stream(4, 5, 6)).toList, List(1, 2, 3, 4, 5, 6))
    assertEquals(
      stream.flatMap(_ => stream).toList,
      List(1, 2, 3, 1, 2, 3, 1, 2, 3)
    )
  }

  test("5.8") {
    assertEquals(Stream.constant(1).take(5).toList, List.fill(5)(1))
    assertEquals(Stream.constant(1).exists(_ == 1), true)
  }

  test("5.9") {
    assertEquals(Stream.from(1).take(5).toList, List(1, 2, 3, 4, 5))
  }

  test("5.10") {
    assertEquals(Stream.fibs.take(8).toList, List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  test("5.11") {
    val ints = Stream.unfold(0)(n => Some((n, n + 1)))
    assertEquals(ints.take(5).toList, List(0, 1, 2, 3, 4))
    val countDown =
      Stream.unfold(5)(n => if (n == 0) None else Some((n, n - 1)))
    assertEquals(countDown.toList, List(5, 4, 3, 2, 1))
  }

  test("5.12") {
    assertEquals(Stream.fibs2.take(20).toList, Stream.fibs.take(20).toList)
    assertEquals(
      Stream.from2(2).take(10).toList,
      Stream.from(2).take(10).toList
    )
    assertEquals(Stream.constant2(5).take(5).toList, List.fill(5)(5))
    assertEquals(Stream.ones2.take(5).toList, List.fill(5)(1))
  }

  test("5.13") {
    assertEquals(stream.map2(_ + 1).toList, List(2, 3, 4))
    assertEquals(stream.take2(2).toList, List(1, 2))
    assertEquals(stream.takeWhile3(_ < 3).toList, List(1, 2))
    assertEquals(stream.zipWith(stream)(_ + _).toList, List(2, 4, 6))
    assertEquals(
      stream.zipAll(Stream(1, 2, 3, 4, 5)).toList,
      List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3)),
        (None, Some(4)),
        (None, Some(5))
      )
    )
  }

  test("5.14") {
    assertEquals(stream.startsWith(Stream(1, 2)), true)
    assertEquals(stream.startsWith(Stream(1, 3)), false)
  }

  test("5.15") {
    assertEquals(
      stream.tails.toList.map(_.toList),
      List(List(1, 2, 3), List(2, 3), List(3))
    )
    assertEquals(stream.hasSubsequence(Stream(2, 3)), true)
  }

  test("5.16") {
    assertEquals(stream.scanRight(0)(_ + _).toList, List(6, 5, 3, 0))
  }
}
