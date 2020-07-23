package zone.slice.fpinscala.chapter2

object IsSorted {
  // Exercise 2.2
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean =
      if (i == as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else go(i + 1)
    go(0)
  }
}
