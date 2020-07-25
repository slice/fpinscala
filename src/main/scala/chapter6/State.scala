package zone.slice.fpinscala.chapter6

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = f(a).run(s2)
      (b, s3)
    }
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s.map(f(a, _)))
}

object State {
  // Exercise 6.10
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss match {
      case Nil    => unit(Nil)
      case h :: t => h.flatMap(a => sequence(t).map(a :: _))
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}
