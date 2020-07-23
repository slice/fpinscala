package zone.slice.fpinscala.chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] =
    this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

  // Exercise 5.1
  def toList: List[A] = {
    @scala.annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty      => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }
    // we should probably not do this
    // update: there's no better way other than a list buffer, so...
    go(this, Nil).reverse
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else
      this match {
        case Empty      => Empty
        case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      }
  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else
      this match {
        case Empty      => Empty
        case Cons(_, t) => t().drop(n - 1)
      }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _                    => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _          => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => if (p(a)) b else false)

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else Empty
    )

  // Exercise 5.6
  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
