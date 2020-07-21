sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

trait ProvidedList {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

trait ListExercises {
  // Exercise 3.2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Cons(_, as) => as
      case Nil => ???
    }

  // Exercise 3.3
  def setHead[A](as: List[A], a: A): List[A] =
    as match {
      case Cons(_, as) => Cons(a, as)
      case Nil => List(a)
    }
}

object List extends ProvidedList with ListExercises

object Program extends App {
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(s"x = $x")

  val three = List(1, 2, 3)

  assert(List.tail(three) == List(2, 3))
  assert(List.setHead(three, 2) == List(2, 2, 3))
}
