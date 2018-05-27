package chapter2

object Exersises {

  def fib(n: Int): Int = {
    def loop(n: Int, first: Int, second: Int): Int =
      if (n <= 0) first
      else loop(n-1, second, first+second)
    loop(n,0,1)
  }
}

object TestFib {

  import Exersises._

  // test implementation of `fib`
  def main(args: Array[String]): Unit = {
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
  }
}