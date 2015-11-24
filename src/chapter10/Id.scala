package chapter10

case class Id[V](value: V) extends Monad[Id] {
  override def unit[A](a: => A): Id[A] = Id(a)
  override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)

  // def unit[W](w: => W) = this.unit(w)
  def flatMap[W](f: V => Id[W]): Id[W] = flatMap(this)(f)
}
