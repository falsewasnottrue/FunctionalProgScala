package chapter10

import org.specs2.mutable.Specification
import Monoid._

class FoldableSpec extends Specification {

  "LFoldable" should {
    "concatenate" in {
      val ls = List("abc", "de", "fgh")
      ListFoldable.concatenate(ls)(stringMonoid) must equalTo("abcdefgh")
    }
  }

  /**
  "TreeFoldable" should {
    "foldLeft" in {
      val tree = Leaf(0)
      TreeFoldable.foldLeft(tree)(intAddition.zero)(intAddition.op) must equalTo(0)
    }
  }
  */

  "OptionFoldable" should {
    "foldLeft" in {
      OptionFoldable.foldLeft[Int, Int](None)(0)(_+_) must equalTo(0)
      OptionFoldable.foldLeft[Int, Int](Some(6))(1)(_+_) must equalTo(7)
    }

    "foldRight" in {
      OptionFoldable.foldRight[Int, Int](None)(0)(_+_) must equalTo(0)
      OptionFoldable.foldRight[Int, Int](Some(6))(1)(_+_) must equalTo(7)
    }

    "foldMap" in {
      OptionFoldable.foldMap[Int, String](None)(_.toString)(stringMonoid) must equalTo("")
      OptionFoldable.foldMap[Int, String](Some(7))(_.toString)(stringMonoid) must equalTo("7")
    }
  }
}
