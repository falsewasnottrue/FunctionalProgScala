package chapter10

import language.higherKinds
import language.reflectiveCalls

trait Functor[M[_]] {
  def map[A,B](ma: M[A])(f: A => B): M[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)
}

class EitherMonad[E] extends Monad[({type f[x] = Either[E, x]})#f] {
  override def unit[A](a: => A): Either[E, A] = Right(a)
  override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = ma match {
    case Right(a) => f(a)
    case Left(e) => Left[E,B](e)
  }
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: => A): Option[A] = Some(a)
  override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
}