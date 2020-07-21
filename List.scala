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

  // p. 36
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  // p. 39
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)
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

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] =
    if (n == 0) as
    else drop(tail(as), n - 1)

  // Exercise 3.5
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case as => as
    }

  // Exercise 3.6
  def init[A](as: List[A]): List[A] = {
    def go(acc: List[A], cur: List[A]): List[A] =
      cur match {
        case Cons(h, t: Cons[A]) => go(List.append(acc, List(h)), t)
        case Cons(h, Nil) => acc
        case Nil => ???
      }
    go(Nil, as)
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(as: List[A], acc: B): B = {
      as match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }
    go(as, z)
  }

  // Exercise 3.11
  def sum3(is: List[Int]): Int =
    foldLeft(is, 0)(_ + _)
  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)
  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go(as: List[A], acc: List[A]): List[A] = {
      as match {
        case Nil => acc
        case Cons(h, t) => go(t, Cons(h, acc))
      }
    }
    go(as, Nil)
  }
  def reverse2[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, elem) => Cons(elem, acc))
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
  val threeD = List(1.0, 2.0, 3.0)

  def t[A](left: A, right: A): Unit = {
    if (left == right) {
      return
    }

    import scala.io.{AnsiColor => Color}
    val message = s"failed: expected $right, got $left"
    Console.err.println(s"${Color.RED}$message${Color.RESET}")
  }

  t(List.tail(three), List(2, 3))
  t(List.setHead(three, 2), List(2, 2, 3))
  t(List.drop(three, 1), List.tail(three))
  t(List.drop(three, 2), List(3))
  t(List.dropWhile(three)(_ < 3), List(3))
  t(List.init(three), List(1, 2))
  t(List.length(three), 3)
  t(List.sum3(three), 6)
  t(List.product3(threeD), 6)
  t(List.length2(three), 3)
  t(List.reverse(three), List(3, 2, 1))
  t(List.reverse2(three), List(3, 2, 1))
}
