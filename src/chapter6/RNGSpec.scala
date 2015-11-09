package chapter6

import org.specs2.mutable.Specification

class RNGSpec extends Specification {

  "RNG" should {
    // ecxercise 6.1
    "generate positive integers" in {
      val rng = RNG.simple(System.currentTimeMillis())
      RNG.positiveInt(rng)._1 must be_>=(0)
    }
  }
}
