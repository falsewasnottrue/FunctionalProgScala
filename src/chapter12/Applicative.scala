package chapter12

import language.higherKinds

import chapter10.Functor

trait Applicative[F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val f2 = map(fa)(f.curried)
    apply(f2)(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def unit[A](a: A): F[A]
}