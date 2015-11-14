package chapter6

import org.specs2.mutable.Specification

class StateSpec extends Specification {

  "State" should {
    "implement unit" in {
      val u = State.constant(1).unit(47.11)
      u.run()._1 must equalTo(47.11)
    }

    "implement map" in {
      val mapped = State.constant(1).map(_ + 1)
      mapped.run()._1 must equalTo(2)
    }

    "implement flatMap" in {
      val flatMapped = State.constant(1).flatMap(i => State.constant(i + 46.11))
      flatMapped.run()._1 must equalTo(47.11)
    }

    "implement map2" in {
      val s1: State[Unit,Int] = State.constant(1)
      val s2: State[Unit,Int] = State.constant(2)
      val m: State[Unit, Int] = State.map2(s1, s2)(_+_)
      m.run()._1 must equalTo(3)
    }

    "implement sequence" in {
      val s1 = State.constant(1)
      val s2 = State.constant(2)
      val s3 = State.constant(3)
      val s4 = State.constant(4)
      val s: State[Unit, List[Int]] = State.sequence(List(s1,s2,s3,s4))
      s.run()._1 must equalTo(List(1,2,3,4))
    }
  }
}
