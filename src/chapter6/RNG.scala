package chapter6

trait RNG {
  def nextInt: (Int, RNG)

}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    val positiveInt = if (i == Int.MinValue) (i+1).abs else i.abs
    (positiveInt, nextRng)
  }
}