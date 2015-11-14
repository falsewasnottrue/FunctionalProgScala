package chapter7

import java.util.concurrent.ExecutorService

import scala.concurrent.Future


object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => Future.successful(a)
  def fork[A](a: => Par[A]): Par[A] = es => a(es)
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => for {
    resA <- a(es)
    resB <- b(es)
  } yield f(resA, resB)

  def async[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
}
