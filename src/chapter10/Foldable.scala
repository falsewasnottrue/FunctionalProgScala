package chapter10

import language.higherKinds

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(Nil: List[A]){
    case (ls, a) => ls :+ a
  }
}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B =
    as.map(f(_)).foldLeft(m.zero)(m.op)

  override def toList[A](fa: List[A]): List[A] = fa
}

sealed trait Tree[+A] {
  def map[B](f: A => B): Tree[B]
}
case class Leaf[A](value: A) extends Tree[A] {
  def map[B](f: A => B): Tree[B] = Leaf(f(value))
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def map[B](f: A => B): Tree[B] = Branch(left.map(f), right.map(f))
}

object TreeFoldable extends Foldable[Tree] {

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = foldLeftAcc(z)(as)(f)

  private def foldLeftAcc[A,B](acc: B)(as: Tree[A])(f: (B,A) => B): B = as match {
    case Leaf(value) => f(acc, value)
    case Branch(left, right) =>
      foldLeftAcc(foldLeftAcc(acc)(left)(f))(right)(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    foldRightAcc(z)(as)(f)

  private def foldRightAcc[A,B](acc: B)(as: Tree[A])(f: (A,B) => B): B = as match {
    case Leaf(value) => f(value, acc)
    case Branch(left, right) =>
      foldRightAcc(foldRightAcc(acc)(right)(f))(left)(f)
  }

  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B =
    foldLeft(as.map(f))(m.zero)(m.op)
}


object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a,z)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z,a)
  }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]): B = as match {
    case None => m.zero
    case Some(a) => m.op(f(a), m.zero)
  }
}

object EitherFoldable extends Foldable[({type eitherM[A] = Either[String,A]})#eitherM] {

  override def foldLeft[A, B](as: Either[String, A])(z: B)(f: (B, A) => B): B = as match {
    case Left(_) => z
    case Right(a) => f(z,a)
  }

  override def foldRight[A, B](as: Either[String, A])(z: B)(f: (A, B) => B): B = as match {
    case Left(_) => z
    case Right(a) => f(a,z)
  }

  override def foldMap[A, B](as: Either[String, A])(f: (A) => B)(m: Monoid[B]): B = as match {
    case Left(_) => m.zero
    case Right(a) => m.op(m.zero, f(a))
  }
}