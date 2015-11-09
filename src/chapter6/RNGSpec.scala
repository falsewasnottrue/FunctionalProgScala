package chapter6

import org.specs2.mutable.Specification

class RNGSpec extends Specification {

  "RNG" should {
    // ecxercise 6.1
    "generate positive integers" in {
      val rng = RNG.simple(System.currentTimeMillis())
      RNG.positiveInt(rng)._1 must be_>=(0)
    }

    // exercise 6.2
    "generate double between 0 and 1" in {
      val rng = RNG.simple(System.currentTimeMillis())
      val rndDouble = RNG.double(rng)._1
      rndDouble must be_>=(0.0)
      rndDouble must be_<=(1.0)
    }

    // exercise 6.4
    "generate list of random ints" in {
      val rng = RNG.simple(System.currentTimeMillis())
      val is = RNG.ints(4)(rng)

      is._1.length must equalTo(4)
      is._2 must be_!=(rng)
    }

    // exercise 6.5
    "positiveMax through map" in {
      val rng = RNG.simple(System.currentTimeMillis())
      val res = RNG.positiveMax(4)(rng)

      res._1 must be_>=(0)
      res._1 must be_<=(4)
    }
  }
}
