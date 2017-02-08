package chapter5

import Stream._

/**
  * Created by Kirill on 2/8/2017.
  */
sealed abstract class Stream[+A] {

  def uncons: Option[Cons[A]]

  def isEmpty = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case None => z
    case Some(c) => f(c.head, c.tail.foldRight(z)(f))
  }

  def exist(p: (A) => Boolean) = foldRight(false)((a, b) => p(a) || b)

  def takeWhile(p: A => Boolean): Stream[A] = {
    val z: Stream[A] = Empty

    foldRight(z)((a, b) => {
      if (p(a)) {
        cons(a, b)
      } else b
    })
  }

  def toList: List[A] = {
    val z: List[A] = Nil
    foldRight(z)((a, b) => List(a) ++ b)
  }

}

object Empty extends Stream[Nothing] {
  val uncons = None
}

sealed abstract class Cons[+A] extends Stream[A] {
  def head: A

  def tail: Stream[A]

  val uncons = Some(this)
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  def toList[A](st: Stream[A]): List[A] = {
    st.uncons match {
      case None => Nil
      case Some(stream) => List(stream.head) ++ toList(stream.tail)
    }
  }

  def take[A](st: Stream[A], n: Int): Stream[A] = {
    def loop(st: Stream[A], counter: Int): Stream[A] = {
      st.uncons match {
        case None => Empty
        case Some(stream) => if (counter == n) Empty else cons(stream.head, loop(stream.tail, counter + 1))
      }
    }

    loop(st, 0)
  }


  def takeWhile[A](st: Stream[A], p: A => Boolean): Stream[A] = {
    st.uncons match {
      case None => Empty
      case Some(stream) => if (p(stream.head)) cons(stream.head, takeWhile(stream.tail, p)) else Empty
    }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }
}


