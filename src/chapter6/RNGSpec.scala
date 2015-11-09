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
      val is = RNG.intsViaSequence(4)(rng)

      is._1.length must equalTo(4)
      is._2 must be_!=(rng)
    }

    // exercise 6.10
    "map through flatMap" in {
      val s: Rand[Int] = RNG.positiveInt

      val m1 = RNG.map(s)(i => i * 2)
      val m2 = RNG.mapViaFlatMap(s)(i => i * 2)

      m1(rng)._1 must equalTo(m2(rng)._1)
    }

    "map2 through flatMap" in {
      val s: Rand[Int] = RNG.positiveInt
      val d: Rand[Double] = RNG.double

      val m1 = RNG.map2(s, d) { case (i,d) => (i,d) }
      val m2 = RNG.map2ViaFlatMap(s,d) { case (i,d) => (i,d) }

      m1(rng)._1 must equalTo(m2(rng)._1)
    }
  }
}
