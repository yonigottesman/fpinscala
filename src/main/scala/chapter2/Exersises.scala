package chapter2

object Exersises {

  def fib(n: Int): Int = {
    def loop(n: Int, first: Int, second: Int): Int =
      if (n <= 0) first
      else loop(n-1, second, first+second)
    loop(n,0,1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int, previous: A): Boolean =
      if (n >= as.length) true
      else if (!ordered(previous,as(n))) false
      else loop(n+1, as(n))
    loop(1, as(0))
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a) => (b => f(a,b))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a=>f(g(a))

}
object TestCurry {
  import Exersises._

  def main(args: Array[String]): Unit = {

  }
}


object TestSorted {
  import Exersises._

  def main(args: Array[String]): Unit = {
    println(isSorted[Int](Array(3,2,3,4,5), (x,y) => x <= y))
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