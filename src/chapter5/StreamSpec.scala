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

    // exercise 5.3

    "takeWhile with positive predicate" in {
      Stream(1,2,3).takeWhile(i => i % 2 == 1).toList must equalTo(List(1))
    }

    "takeWhile with negative predicate" in {
      Stream(1,2,3).takeWhile(i => i % 2 == 0).toList must equalTo(Nil)
    }

    // exercise 5.4

    "forAll yields true" in  {
      Stream(1,2,3).forAll(i => i > 0) must equalTo(true)
    }

    "forAll yields false" in {
      Stream(1,2,-3).forAll(i => i > 0) must equalTo(false)
    }

    "all predicates are true over an empty set" in {
      Stream.empty[Int].forAll(i => i != i) must equalTo(true)
    }

    // exercise 5.6

    "headOption for empty list returns None" in {
      Stream.empty.headOption must equalTo(None)
    }

    "headOption for non-empty list returns first element" in {
      Stream(1,2,3).headOption must equalTo(Some(1))
    }

    // exercise 5.7

    "map is implemented by foldRight" in {
      Stream(1,2,3).map(_ + 1).toList must equalTo(List(2,3,4))
    }

    "flatMap is implemented by foldRight" in {
      Stream(1,2,3).flatMap(i => Stream(i.toString)).toList must equalTo(List("1", "2", "3"))
    }

    "filter is implemented by foldRight" in {
      Stream(1,2,3).filter(i => i % 2 == 0).toList must equalTo(List(2))
    }

    "append is implemented by foldRight" in {
      Stream(1,2,3).append(Stream(4,5,6)).toList must equalTo(List(1,2,3,4,5,6))
    }

    // exercise 5.8

    "constant stream" in {
      Stream.constant(1).take(3).toList must equalTo(List(1,1,1))
    }

    // exercise 5.9

    "infinite stream of increasing integers" in {
      Stream.from(3).take(3).toList must equalTo(List(3,4,5))
    }

    // exercise 5.10

    "fibonacci stream" in {
      Stream.fibs.take(6).toList must equalTo(List(0,1,1,2,3,5))
    }
  }
}
