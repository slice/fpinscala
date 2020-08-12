package zone.slice.fpinscala
package chapter9

import scala.util.matching.Regex

import chapter8._

trait Parsers[ParseError, Parser[+_]] { self =>
  // use parser ops on parsers
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  // using regexes to parse
  implicit def regex(r: Regex): Parser[String]

  // use strings as parsers
  implicit def stringToStringParser(s: String): Parser[String] = string(s)
  // use parser ops on strings
  implicit def asStringOperators[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // making parsers
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  def string(s: String): Parser[String]
  def pure[A](a: A): Parser[A] =
    string("").as(a)

  // combinators
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  // Exercise 9.4 (going to preserve method def order here)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, if (n == 0) pure(Nil) else listOfN(n - 1, p))(_ :: _)
  // Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p) | pure(Nil))(_ :: _)
  // map2(p, many(p))(_ :: _) | pure(Nil)
  // quick question: is `map2(p, a | b))(c)` === `map2(p, a)(c) | b`?? :think:
  // ... we need to make `map2` and `product` non-strict in their second arguments
  //     or else they'll just evaluate forever.
  // Exercise 9.1
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)
  // Exercise 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => pure(f(a)))

  /**
    * Parses to the input instead of the parsed result.
    */
  def slice[A](p: Parser[A]): Parser[String]
  // "Conceptually, `product` should have been non-strict in its second argument
  //  anyway, since if the first Parser fails, the second won't even be
  //  consulted."
  // Exercise 9.7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => flatMap(p2)(b => pure(a, b)))
  // Exercise 9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2) map (f.tupled)
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  // Exercise 9.7
  def map2UsingFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => flatMap(p2)(b => pure(f(a, b)))) // boooooring! just `product` then `map`

  // Exercise 9.6
  def specifiedA: Parser[List[String]] =
    "[0-9]".r.flatMap(ns => listOfN(ns.toInt, "a"))

  def as[A, B](p: Parser[A])(b: B): Parser[B] =
    map(p)(_ => b)
  def then[A, B](p: Parser[A], p2: Parser[B]): Parser[B] =
    flatMap(p)(_ => p2)
  def thenL[A, B](p: Parser[A], p2: Parser[B]): Parser[A] =
    flatMap(p)(pa => map(p2)(_ => pa))

  // maybe this should be a value class?...
  case class ParserOps[A](val p: Parser[A]) {
    def run(input: String): Either[ParseError, A] = self.run(p)(input)
    def |[B >: A](p2: Parser[B]): Parser[B]       = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B]      = self.or(p, p2)
    def listOfN(n: Int): Parser[List[A]]          = self.listOfN(n, p)
    def *(n: Int): Parser[List[A]]                = self.listOfN(n, p)
    def many: Parser[List[A]]                     = self.many(p)
    def `?` : Parser[List[A]]                     = self.many(p)
    def many1: Parser[List[A]]                    = self.many1(p)
    def `!` : Parser[List[A]]                     = self.many1(p)
    def map[B](f: A => B): Parser[B]              = self.map(p)(f)
    def slice: Parser[String]                     = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)]      = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
      self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def as[B](b: B): Parser[B]                   = self.as(p)(b)
    def then[B](p2: Parser[B]): Parser[B]        = self.then(p, p2)
    def *>[B](p2: Parser[B]): Parser[B]          = self.then(p, p2)
    def thenL[B](p2: Parser[B]): Parser[A]       = self.thenL(p, p2)
    def <*[B](p2: Parser[B]): Parser[A]          = self.thenL(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    // Exercise 9.2
    def productLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p ** p, p.map(a => (a, a)))(in)
  }
}

sealed trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // Exercise 9.9
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    def digit: Parser[String] = "[0-9]".r
    def number: Parser[Int] = digit.many1.slice.map(_.toInt)
    def quote: Parser[String] = "\"" // lol
    def any: Parser[String] = ".".r

    def jnull: Parser[JNull.type] = "null".as(JNull)
    def jnumber: Parser[JNumber] = number.map(JNumber(_))
    def jstring: Parser[JString] = (quote *> any.many.slice <* quote).map(JString(_))
    def jbool: Parser[JBool] = ("true" | "false").map(s => JBool(s == "true"))
    def jarray: Parser[JArray] = ("[" *> json.many <* "]").map(os => JArray(os.toVector))
    def jobject: Parser[JObject] =
      ("{" *> (jstring ** (":" *> json)).many <* "}")
        .map(_.map { case (key, json) => key.get -> json }.toMap).map(JObject(_))
    def json: Parser[JSON] = jnull | jnumber | jstring | jbool | jarray | jobject

    json
  }
}
