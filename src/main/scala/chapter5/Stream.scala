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

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  def append[AA >: A](o: => Stream[AA]): Stream[AA] =
    foldRight(o)((a, b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  // Exercise 5.13
  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None
    }
  def take2(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (0, _)          => None
      case (n, Cons(h, t)) => Some((h(), (n - 1, t())))
      case (_, Empty)      => None
    }
  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }
  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, b)) {
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
      case _                          => None
    }
  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, b)) {
      case (Cons(h, t), Cons(h2, t2)) =>
        Some((Some(h()), Some(h2())), (t(), t2()))
      case (Cons(h, t), Empty) =>
        Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) =>
        Some((None, Some(h())), (Empty, t()))
      case (Empty, Empty) =>
        None
    }

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    Stream
      .unfold((this, s)) {
        case (Cons(hh, ht), Cons(nh, nt)) =>
          Some((hh() == nh(), (ht(), nt())))
        case _ => None
      }
      .takeWhile(identity)
      .toList
      .size >= s.toList.size

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case _              => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.tails.map(_.foldRight(z)(f)).append(Stream(z))
}

case object Empty                                   extends Stream[Nothing]
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

  // Exercise 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a + b, go(b, a + b))
    Stream.cons(0, Stream.cons(1, go(0, 1)))
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((value, state)) => Stream.cons(value, unfold(state)(f))
      case None                 => Empty
    }

  // Exercise 5.12
  def fibs2: Stream[Int] =
    Stream(0, 1).append(unfold((0, 1)) {
      case (l, r) => Some((l + r, (r, l + r)))
    })
  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))
  def constant2[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))
  def ones2: Stream[Int] =
    constant2(1)
}
