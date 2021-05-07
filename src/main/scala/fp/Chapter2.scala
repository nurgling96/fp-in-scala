package fp

object Chapter2 {
  // Task 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)
    }

    loop(n, 0, 1)
  }

  // Task 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (as.length - 1 == n) true
      else if (!ordered(as(n + 1), as(n))) false
      else loop(n + 1)

    loop(0)
  }

  // Task 2.3
  def curry[A, B, C](f: (A, B) => C): A => B =>  C = a => b => f(a, b)

  // Task 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Task 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
