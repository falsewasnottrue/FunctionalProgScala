package chapter10

import org.scalacheck.{Gen, Arbitrary}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import Monoid._

class MonoidSpec extends Specification with ScalaCheck {

  "Monoid" should {
    "intAddition is a Monoid" in {
      implicit val arbitraryInt = Arbitrary.arbitrary[Int]

      prop { (a1: Int, a2: Int) =>
        intAddition.op(a1, a2) must equalTo(intAddition.op(a2,a1))
        intAddition.op(a1, intAddition.zero) must equalTo(a1)
        intAddition.op(intAddition.zero, a2) must equalTo(a2)
      }
    }

    "booleanOr is a Monoid" in {
      implicit val arbitraryBoolean = Arbitrary.arbitrary[Boolean]

      prop { (a1: Boolean, a2: Boolean) =>
        booleanOr.op(a1, a2) must equalTo(booleanOr.op(a2,a1))
        booleanOr.op(a1, booleanOr.zero) must equalTo(a1)
        booleanOr.op(booleanOr.zero, a2) must equalTo(a2)
      }
    }

  }
}
