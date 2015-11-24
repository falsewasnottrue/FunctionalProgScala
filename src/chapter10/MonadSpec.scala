package chapter10

import org.specs2.mutable.Specification

class MonadSpec extends Specification {
  "OptionMonad" should {
    "implement map" in {
      OptionMonad.map(Some(3))(_+1) must equalTo(Some(4))
      OptionMonad.map[Int,Int](None)(_+1) must equalTo(None)
    }
  }
}
