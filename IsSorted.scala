object IsSorted extends App {
  // Exercise 2.2
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean =
      if (i == as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else go(i + 1)
    go(0)
  }

  println(isSorted(List(1, 2, 3, 4, 5))(_ < _))
  println(isSorted(List(5, 4, 3, 1, 2))(_ < _))
}
