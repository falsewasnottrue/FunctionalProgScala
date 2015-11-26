package chapter12

import language.higherKinds

import chapter10.Functor

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: A): F[A]

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val f2 = map(fa)(f.curried)
    apply(f2)(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map[K,V]())) {
      case (acc, (k, fv)) =>
        map2(map(fv)(v => Map(k,v)), acc)(_ ++ _)
    }
  }
}