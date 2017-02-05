package chaptertwo

import scala.annotation.tailrec

/**
  * Created by Kirill on 1/30/2017.
  */
object MyModule {

  def abs(value: Int): Int = if (value < 0) -value else value


  private def formatAbs(x: Int) = {
    val message = "The absolute value of %d is %d"
    message.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int) = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def main(args: Array[String]) = {
    println(formatAbs(-7))
  }

  def findFirst[T](ds: Array[T], f: T => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ds.length - 1) -1
      else if (f(ds(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], f: (A, A) => Boolean) = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length - 1) true
      else if (f(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: (A) => B, g: B => C): A => C = {
    (a: A) => g(f(a))
  }

}
