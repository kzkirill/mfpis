package chapter3.datastructures

import scala.annotation.tailrec

/**
  * Created by Kirill on 2/2/2017.
  */

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(list: List[Double]): Double = list match {
    case Nil => 1.0
    case Cons(x, xs) => x + product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
  }

  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)

  def product2(l: List[Double]) = foldRight(l, 1.0)((x, y) => x * y)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

  def tail[A](as: List[A]) = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](as: List[A], newHead: A) = as match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  def drop[A](as: List[A], n: Int) = {

    @tailrec
    def loop[A](as: List[A], counter: Int): List[A] = as match {
      case Nil => Nil
      case Cons(_, tail) => if (counter == n - 1) tail else loop(tail, counter + 1)
    }

    loop(as, 0)
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if (f(x)) => dropWhile(xs)(f)
    case _ => as
  }


  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](list: List[A]): Int = {
    foldRight(list, 0)((_, b: Int) => b + 1)
  }

  def reverse[A](l: List[A]) = {
    foldLeft(l, Nil: List[A])((x, xs) => Cons(x, xs))
  }

  def foldLeftByR[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(head, Nil) => foldRight(Nil, f(head, z))(f)
    case Cons(head, tail) => foldLeftByR(tail, f(head, z))(f)
  }

  def concat[A](list: List[List[A]]): List[A] = {
    foldRight(list, Nil: List[A])((oneList, acc) => foldRight(oneList, acc)((a, b) => Cons(a, b)))
  }
}

