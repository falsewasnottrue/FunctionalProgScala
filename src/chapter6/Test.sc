import chapter6._

val rng0 = RNG.simple(42)
val (i0, rng1) = rng0.nextInt
val (i1, rng2) = rng1.nextInt
RNG.ints(0)(rng2)