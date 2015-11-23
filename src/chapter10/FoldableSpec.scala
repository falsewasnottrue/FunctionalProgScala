package chapter10

import org.specs2.mutable.Specification
import Monoid._

class FoldableSpec extends Specification {

  "LFoldable" should {
    "concatenate" in {
      val ls = List("abc", "de", "fgh")
      LFoldable.concatenate(ls)(stringMonoid) must equalTo("abcdefgh")
    }
  }
}
