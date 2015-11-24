package chapter10

case class Id[A](value: A) extends Monad[Id] {
  override def unit[A](a: => A): Id[A] = Id(a)
  override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = f(ma.value)
}
