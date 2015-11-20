package chapter10

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount