package chapter8

class Gen[A] {

}

object Gen {
  def forAll[A](a: Gen[A])(f: A => Boolean) = ???
}