object Fib extends App {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) a
      else go(n - 1, b, a + b)
    go(n, 0, 1)
  }

  for (n <- 0 until 10) {
    println(s"fib($n) = ${fib(n)}")
  }
}
