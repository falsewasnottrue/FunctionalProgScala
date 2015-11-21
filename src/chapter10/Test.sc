import chapter10.Monoid._
val s = List("abc", "def", "ghi")

s.foldLeft(stringMonoid.zero)(stringMonoid.op)
s.foldRight(stringMonoid.zero)(stringMonoid.op)