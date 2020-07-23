package zone.slice.fpinscala.chapter2

object Currying {
  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
