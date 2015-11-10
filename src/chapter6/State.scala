package chapter6

case class State[S,+A](run: S => (A,S)) {

  def unit[B >: A](a: B): State[S,B] = State(s => (a,s))

  def map[B](f: A => B): State[S,B] = {
    State(s => {
      val (a,n) = run(s)
      (f(a), n)
    })
  }

  def flatMap[B](f: A => State[S,B]): State[S,B] = {
    State(s => {
      val (a,n) = run(s)
      f(a).run(n)
    })
  }
}

object State {
  def map2[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] =
    sa.flatMap(a => sb.map(b => f(a,b)))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = fs match {
    case Nil => throw new IllegalArgumentException
    case f :: Nil => f.map(a => List(a))
    case f :: fss => f.flatMap(a => sequence(fss).map(as => a :: as))
  }

  def constant[A](a: A) = State[Unit, A](_ => (a, ()))
}

