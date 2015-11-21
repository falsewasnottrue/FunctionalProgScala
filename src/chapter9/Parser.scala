package chapter9

class Parser[A]

trait Parsers[ParseError, Parser[_]] {
  def run[A](parser: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
}
