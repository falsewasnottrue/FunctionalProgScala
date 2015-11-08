package chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight[Option[A]](None)((hd, tl) => Some(hd))

  /**
  this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  */

  def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
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

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream.empty)((hd,tl) => if (p(hd)) Stream.cons(hd, tl.takeWhile(p)) else Stream.empty)

  /**
  this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }
  */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h,t) if !p(h()) => false
    case Cons(h,t) => t().forAll(p)
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
