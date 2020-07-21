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

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    List.foldRight(a1, a2)((elem, acc) => Cons(elem, acc))

  // Exercise 3.15
  def flatten[A](as: List[List[A]]): List[A] = {
    @scala.annotation.tailrec
    def go(as: List[List[A]], acc: List[A]): List[A] =
      as match {
        case Cons(h, t) => go(t, List.append(h, acc))
        case Nil => acc
      }
    go(as, Nil)
  }
  // actually, let's try using a fold...
  def flatten2[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A])((acc, elem) => List.append(elem, acc))

  // Exercise 3.16
  def add1(is: List[Int]): List[Int] =
    List.foldRight(is, Nil: List[Int])((elem, acc) => Cons(elem + 1, acc))

  // Exercise 3.17
  def doublesToString(ds: List[Double]): List[String] =
    List.foldRight(ds, Nil: List[String])((elem, acc) => Cons(elem.toString, acc))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])((elem, acc) => Cons(f(elem), acc))
  // probably not great because `List.foldRight` isn't stack-safe, so let's...
  def map2[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Cons(h, t) => Cons(f(h), map(t)(f))
      case Nil => Nil
    }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
      case Nil => Nil
    }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Cons(h, t) => List.append(f(h), flatMap(t)(f))
      case Nil => Nil
    }

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(elem => if (f(elem)) List(elem) else List())

  // Exercise 3.22
  def addCorresponding(is1: List[Int], is2: List[Int]): List[Int] =
    (is1, is2) match {
      case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, addCorresponding(t, t2))
      case _ => Nil
    }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(h, t), Cons(h2, t2)) =>
        Cons(f(h, h2), zipWith(t, t2)(f))
      case _ => Nil
    }

  // Exercise 3.24
  def hasSubsequence[A](haystack: List[A], needle: List[A]): Boolean = {
    @scala.annotation.tailrec
    def sequenceMatches(haystack: List[A], needle: List[A]): Boolean =
      (haystack, needle) match {
        case (Cons(h, _), Cons(h2, Nil)) if h == h2 => true
        case (Cons(h, t: Cons[A]), Cons(h2, t2: Cons[A])) if h == h2 =>
          sequenceMatches(t, t2)
        case _ => false
      }

    val Cons(nh, _) = needle

    @scala.annotation.tailrec
    def scan(as: List[A]): Boolean =
      as match {
        case Cons(h, t) =>
          if (h == nh && sequenceMatches(as, needle)) true
          else scan(t)
        case _ => false
      }
    scan(haystack)
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
  val threeD = List(1.0, 2.0, 3.0)

  def t[A](left: A, right: A): Unit = {
    if (left == right) {
      return
    }

    import scala.io.{AnsiColor => Color}
    val message = s"failed: expected $right, got $left"
    Console.err.println(s"${Color.RED}$message${Color.RESET}")
    System.exit(1)
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
  t(List.append2(three, three), List(1, 2, 3, 1, 2, 3))
  t(List.flatten(List(three, three, three)), List(1, 2, 3, 1, 2, 3, 1, 2, 3))
  t(List.flatten2(List(three, three, three)), List(1, 2, 3, 1, 2, 3, 1, 2, 3))
  t(List.add1(three), List(2, 3, 4))
  t(List.doublesToString(threeD), List("1.0", "2.0", "3.0"))
  t(List.map(three)(_ + 1), List(2, 3, 4))
  t(List.map2(three)(_ + 1), List(2, 3, 4))
  t(List.filter(three)(_ == 2), List(2))
  t(List.flatMap(three)(n => List(n, n)), List(1, 1, 2, 2, 3, 3))
  t(List.filter2(three)(_ == 2), List(2))
  t(List.addCorresponding(three, three), List(2, 4, 6))
  t(List.zipWith(three, three)(_ + _), List.addCorresponding(three, three))
  t(List.hasSubsequence(three, List(2, 3)), true)
  t(List.hasSubsequence(List(1, 2, 3, 4, 5, 6, 5, 4), List(5, 6, 5)), true)
}
