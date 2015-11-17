package chapter8

import chapter6._

case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](
    State[RNG, Int](rng => {
      val (nextInt, nextRng) = rng.nextInt
      val res = (nextInt + start) % stopExclusive
      (res, nextRng)
    })
  )

  def unit[A](a: => A): Gen[A] = new Gen[A](
    State[RNG, A](rng => (a, rng))
  )

  def boolean: Gen[Boolean] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
}