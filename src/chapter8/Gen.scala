package chapter8

object Gen {
  import chapter6._
  type Gen[A] = State[RNG,A]

  def choose(start: Int, stopExclusive: Int): Gen[Int] = State[RNG,Int](rng => {
    val (res, nextRng) = rng.nextInt
    val i = (res + start) % stopExclusive
    (i, nextRng)
  })
}