package zone.slice.fpinscala.chapter4

trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None    => None
    }
  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(a) => a
      case None    => default
    }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] =
    this.flatMap { value =>
      if (f(value)) Some(value) else None
    }
}

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(av), Some(bv)) => Some(f(av, bv))
      case _                    => None
    }
  // maybe using tuples is cheating here?
  def map2B[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { av => b.map { bv => f(av, bv) } }
  // p. 60
  def map2C[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      av <- a
      bv <- b
    } yield f(av, bv)

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => h.flatMap(v => sequence(t).map(v :: _))
    }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => f(h).flatMap(v => traverse(t)(f).map(v :: _))
    }
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}

case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

object OptionAux {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(xsm => mean(xs.map(x => math.pow(x - xsm, 2))))

  def insuranceRateQuote(age: Int, speedingTickets: Int): Double = 10.0

  def parseInsuranceRateQuote(
      age: String,
      speedingTickets: String
  ): Option[Double] = {
    val optAge: Option[Int]     = Try { age.toInt }
    val optTickets: Option[Int] = Try { speedingTickets.toInt }

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}
