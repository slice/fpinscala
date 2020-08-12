package zone.slice.fpinscala

case class FakeParser[+A]()

class Chapter9Suite extends munit.FunSuite {
  import chapter9._

  test("assumptions") {
    type UsedParser[+A] = FakeParser[A]

    val parsers: Parsers[String, UsedParser] = ???
    import parsers._

    def parsesTo[A](p: UsedParser[A])(input: String)(result: A): Unit =
      assertEquals(parsers.run(p)(input), Right(result))
    def doesntParse[A](p: UsedParser[A])(input: String): Unit =
      assertEquals(parsers.run(p)(input).isLeft, true)

    // run(string(s))(s) == Right(s)

    // `or`
    val abraCadabra = "abra" | "cadabra"
    doesntParse(abraCadabra)("yop")
    parsesTo(abraCadabra)("abra")("abra")
    parsesTo(abraCadabra)("cadabra")("cadabra")

    // `listOfN`
    val cad = ("ab" | "cad") * 3
    parsesTo(cad)("ababcad")(List("ab", "ab", "cad"))
    parsesTo(cad)("cadabab")(List("cad", "ab", "ab"))
    parsesTo(cad)("ababab")(List("ab", "ab", "ab"))

    // `zeroOrMore`
    val zomDot = ".".?
    parsesTo(zomDot)("")(Nil)
    parsesTo(zomDot)(".")(List("."))
    parsesTo(zomDot)("...")(List.fill(3)("."))

    // `many`
    val oomDot = ".".!
    doesntParse(oomDot)("")
    parsesTo(oomDot)(".")(List("."))
    parsesTo(oomDot)("...")(List.fill(3)("."))
  }
}
