package zone.slice.fpinscala.chapter4

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(a)  => Left(a)
    }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(a)  => Left(a)
    }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_)  => b
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

object Either {
  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      // case h :: t => f(h).flatMap(v => traverse(t).map(v :: _))
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }
}

case class Left[+E](value: E)  extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
