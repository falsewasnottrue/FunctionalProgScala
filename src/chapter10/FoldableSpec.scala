package chapter10

import org.specs2.mutable.Specification
import Monoid._

class FoldableSpec extends Specification {

  "ListFoldable" should {
    "concatenate" in {
      val ls = List("abc", "de", "fgh")
      ListFoldable.concatenate(ls)(stringMonoid) must equalTo("abcdefgh")
    }

    "toList" in {
      ListFoldable.toList(Nil) must equalTo(Nil)
      ListFoldable.toList(List(1,2,3)) must equalTo(List(1,2,3))
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

    "toList" in {
      OptionFoldable.toList(None) must equalTo(Nil)
      OptionFoldable.toList(Some(1)) must equalTo(List(1))
    }
  }
}
