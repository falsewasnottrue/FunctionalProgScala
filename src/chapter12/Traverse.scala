package chapter12

import language.higherKinds
import chapter10._

trait Traverse[F[_]] extends Functor[F] {

  def traverse[M[_]:Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)
}

object ListTraverse extends Traverse[List] {
  override def map[A, B](ma: List[A])(f: (A) => B): List[B] = ma map f
}

object OptionTraverse extends Traverse[Option] {
  override def map[A, B](ma: Option[A])(f: (A) => B): Option[B] = ma map f
}

object TreeTraverse extends Traverse[Tree] {
  override def map[A, B](ma: Tree[A])(f: (A) => B): Tree[B] = ma map f
}
