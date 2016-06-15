package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    // Δ = b² - 4ac

    Signal({
      val av = a()
      val bv = b()
      val cv = c()
      bv * bv - 4 * av * cv
    })
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / 2a
    Signal({
      val dv = delta()
      val av = a()
      val bv = b()
      val cv = c()

      val av2 = 2*av

      Math.sqrt(dv) match {
        case dsqrt if dsqrt.isNaN => Set()
        case dsqrt if dsqrt == 0 => Set(-bv / av2)
        case dsqrt => Set((-bv - dsqrt)/av2, (-bv + dsqrt)/av2)
      }
    })
  }
}
