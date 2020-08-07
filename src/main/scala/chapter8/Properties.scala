package zone.slice.fpinscala.chapter8

object Properties {
  // Exercise 8.1
  //
  // What are some possible properties of this sum function?
  //
  // * `sum(list)` should be the same as `foldLeft(list)(_ + _)` and
  //   `foldRight(list)(_ + _)`.
  // * `sum(list.reverse)` should be the same as `sum(list)`.
  // * If all elements of the list are the same, then `sum(list)` should be the
  //   same as `list * list.size`.
  def sum(is: List[Int]): Int = ???

  // Exercise 8.2
  //
  // * The order of the list shouldn't affect the result (for example,
  //   `max(list.reverse)` should be the same as `max(list)`).
  def max(is: List[Int]): Int = ???
}
