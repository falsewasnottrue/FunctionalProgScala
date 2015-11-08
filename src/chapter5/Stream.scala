package chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n == 0) Stream.empty else this match {
      case Empty => Stream.empty
      case Cons(h, t) => Stream.cons(h(), t().take(n-1))
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this else this match {
      case Empty => Stream.empty
      case Cons(h, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }
}

case object Empty extends Stream[Nothing]
case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
}
