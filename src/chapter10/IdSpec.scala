package chapter10

import org.specs2.mutable.Specification

class IdSpec extends Specification {

  "Id" should {
    "be a monad" in {
      //Id("Hello, ") flatMap (a => Id("monad!") flatMap (b => Id(a + b))) must equalTo(Id("Hello, monad!"))
      Id(1) flatMap(a => Id(2) flatMap(b => Id(a+b))) must equalTo(Id(3))
    }
  }
}
