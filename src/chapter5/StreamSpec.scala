package chapter5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "Stream" should {
    "be serializable to list" in {
      Stream(1,2,3).toList must equalTo(List(1,2,3))
    }

    "be serializable to empty list" in {
      Stream.empty.toList must equalTo(Nil)
    }

    "take n from stream longer than n" in {
      Stream(1,2,3).take(2).toList must equalTo(List(1,2))
    }

    "take n from stream shorter than n" in {
      Stream(1,2).take(3).toList must equalTo(List(1,2))
    }

    "take from an empty stream" in {
      Stream.empty.take(3).toList must equalTo(Nil)
    }

    "take 0" in {
      Stream(1,2,3).take(0).toList must equalTo(Nil)
    }

    "drop n from a stream longer than n" in {
      Stream(1,2,3).drop(2).toList must equalTo(List(3))
    }

    "drop n from a stream shorter than n" in {
      Stream(1,2).drop(3).toList must equalTo(Nil)
    }

    "drop from an empty stream" in {
      Stream.empty.drop(3).toList must equalTo(Nil)
    }

    "drop 0" in {
      Stream(1,2,3).drop(0).toList must equalTo(List(1,2,3))
    }
  }
}
