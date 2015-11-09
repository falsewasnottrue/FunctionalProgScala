package chapter6

import chapter6.RNG.Rand
import org.specs2.mutable.Specification

class RNGSpec extends Specification {

  val rng = RNG.simple(System.currentTimeMillis())

  "RNG" should {
    // exercise 6.1
    "generate positive integers" in {
      RNG.positiveInt(rng)._1 must be_>=(0)
    }

    // exercise 6.2
    "generate double between 0 and 1" in {
      val rndDouble = RNG.double(rng)._1
      rndDouble must be_>=(0.0)
      rndDouble must be_<=(1.0)
    }

    // exercise 6.4
    "generate list of random ints" in {
      val is = RNG.ints(4)(rng)

      is._1.length must equalTo(4)
      is._2 must be_!=(rng)
    }

    // exercise 6.5
    "positiveMax through map" in {
      val res = RNG.positiveMax(4)(rng)

      res._1 must be_>=(0)
      res._1 must be_<=(4)
    }

    // exercise 6.6
    "generate double through map" in {
      val rndDouble = RNG.doubleViaMap(rng)._1

      rndDouble must be_>=(0.0)
      rndDouble must be_<=(1.0)
    }

    // exercise 6.7
    "generate (int, double) through map2" in {
      val rndIntDouble = RNG.intDoubleViaMap2(rng)._1

      rndIntDouble._1 must be_>=(1)
      rndIntDouble._2 must be_>=(0.0)
      rndIntDouble._2 must be_<=(1.0)
    }

    // exercise 6.8
    "combine list of Rand generators" in {
      val rndTuple = RNG.sequence(
        List(
          RNG.double: Rand[Double],
          RNG.positiveInt: Rand[Int],
          RNG.double: Rand[Double],
          RNG.positiveInt: Rand[Int]
        )
      )(rng)._1

      rndTuple.size must beEqualTo(4)
      rndTuple(0).isInstanceOf[Double] must beTrue
      rndTuple(1).isInstanceOf[Int] must beTrue
      rndTuple(2).isInstanceOf[Double] must beTrue
      rndTuple(3).isInstanceOf[Int] must beTrue
    }
  }
}
