package chapter7

import java.util.concurrent.ExecutorService

import scala.concurrent.Future


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => Future.successful(a)
  def fork[A](a: => Par[A]): Par[A] = es => a(es)

  def async[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A,B](f: A => B): A => Par[B] = a => async(f(a))

  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = es => for {
    a <- fa(es)
    b <- fb(es)
  } yield (a,b)
  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def map2[A,B,C](fa: Par[A], fb: Par[B])(f: (A,B) => C): Par[C] =
    map(product(fa,fb)) {
      case (a,b) => f(a,b)
    }

  def sorted(l: Par[List[Int]]) = map(l)(_.sorted)

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(parMap(as)(a => if (f(a)) Some(a) else None))(maybeA => maybeA.filter(_.isDefined).map(_.get))

  def sequence[A](l: List[Par[A]]): Par[List[A]] = l match {
    case Nil => unit(Nil)
    case l0 :: ls => map2(l0, sequence(ls))(_ :: _)
  }
}
