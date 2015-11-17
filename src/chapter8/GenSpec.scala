package chapter8

import chapter6.RNG
import org.specs2.mutable.Specification

class GenSpec extends Specification {

  val rng = RNG.simple(System.currentTimeMillis())

  "Gen" should {
    "implement choose" in {
      val gen1: Gen[Int] = Gen.choose(1,10)
      val v = gen1.sample.run(rng)._1

      v must be_>=(1)
      v must be_<(10)
    }
  }
}
