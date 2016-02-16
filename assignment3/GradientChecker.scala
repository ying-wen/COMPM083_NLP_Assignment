package uk.ac.ucl.cs.mr.statnlpbook.assignment3


/**
 * @author: yingwen
 */
object GradientChecker extends App {
  val EPSILON = 1e-6

  /**
   * For an introduction see http://cs231n.github.io/neural-networks-3/#gradcheck
   *
   * This is a basic implementation of gradient checking.
   * It is restricted in that it assumes that the function to test evaluates to a double.
   * Moreover, another constraint is that it always tests by backpropagating a gradient of 1.0.
   */
  def apply[P](model: Block[Double], paramBlock: ParamBlock[P]) = {
    paramBlock.resetGradient()
    model.forward()
    model.backward(1.0)

    var avgError = 0.0

    val gradient = paramBlock.gradParam match {
      case m: Matrix => m.toDenseVector
      case v: Vector => v
    }

    /**
     * Calculates f_theta(x_i + eps)
     * @param index i in x_i
     * @param eps value that is added to x_i
     * @return
     */
    def wiggledForward(index: Int, eps: Double): Double = {
      var result = 0.0
      paramBlock.param match {
        case v: Vector =>
          val tmp = v(index)
          v(index) = tmp + eps
          result = model.forward()
          v(index) = tmp
        case m: Matrix =>
          val (row, col) = m.rowColumnFromLinearIndex(index)
          val tmp = m(row, col)
          m(row, col) = tmp + eps
          result = model.forward()
          m(row, col) = tmp
      }
      result
    }

    for (i <- 0 until gradient.activeSize) {
      //todo: your code goes here!
      val gradientExpected: Double = (wiggledForward(i,EPSILON)-wiggledForward(i,-EPSILON))/(2*EPSILON)
      println(gradientExpected)
      avgError = avgError + math.abs(gradientExpected - gradient(i))
      println(gradientExpected+";"+gradient(i))
      assert(
        math.abs(gradientExpected - gradient(i)) < EPSILON,
        "Gradient check failed!\n" +
          s"Expected gradient for ${i}th component in input is $gradientExpected but I got ${gradient(i)}"
      )
    }

    println("Average error: " + avgError)
  }
  val h = vec(0.01, 0.02,0.03,0.05)
  val x = vec(0.02, 0.01,0.01,0.06)
  val w = VectorParam(4)
  w.set(vec(0.02, 0.02,0.01,0.07))
  val b = VectorParam(4)
  b.set(vec(0.06, 0.07,0.09,0.08))
  val s:Seq[Block[Vector]] = Seq(h,x,b)
  val simpleBlock = Dot(w,PoolingMax(s))
  GradientChecker(simpleBlock, b)

}
