package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.linalg._
import breeze.numerics._

/**
  * @author rockt,yingwen
  */

/**
  * A trait for the core building **block** of our computation graphs
  *
  * @tparam T the type parameter this block evaluates to (can be Double, Vector, Matrix)
  */
trait Block[T] {
  //caches output after call of forward
  var output: T = _
  //fun fact: when you say "forward" or "back" your lips move in the respective direction
  def forward(): T
  //assumes that forward has been called first!
  def backward(gradient: T): Unit
  //updates parameters of the block
  def update(learningRate: Double)
}

/**
  * A loss function is a block that evaluates to a single double.
  * Generally loss functions don't have upstream gradients,
  * so we can provide an implementation of backward without arguments.
  */
trait Loss extends Block[Double] {
  def backward(): Unit
}

trait ParamBlock[P] extends Block[P] {
  var param: P
  val gradParam: P
  def initialize(dist: () => Double): P
  def set(p: P): Unit = {
    param = p
  }
  def resetGradient(): Unit
}

trait DefaultInitialization {
  def defaultInitialization(): Double
}

/**
  * This trait defines a default way of initializing weights of parameters
  */
trait GaussianDefaultInitialization extends DefaultInitialization {
  def defaultInitialization(): Double = random.nextGaussian() * 0.1
}

/**
  * A simple block that represents a constant double value
  *
  * @param arg the constant double value
  */
case class DoubleConstant(arg: Double) extends Block[Double] with Loss {
  output = arg
  def forward(): Double = output
  def backward(gradient: Double): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
  def backward(): Unit = {} //nothing to do since fixed
}

/**
  * A simple block that represents a constant vector
  *
  * @param arg the constant vector
  */
case class VectorConstant(arg: Vector) extends Block[Vector] {
  output = arg
  def forward(): Vector = output
  def backward(gradient: Vector): Unit = {} //nothing to do since fixed
  def update(learningRate: Double): Unit = {} //nothing to do since fixed and no child blocks
}

/**
  * A block representing a sum of doubles
  *
  * @param args a sequence of blocks that evaluate to doubles
  */
case class DoubleSum(args: Block[Double]*) extends Block[Double] {
  def forward(): Double = {
    if(output==null)
      output = args.map(_.forward()).sum
    output
  }
  def backward(gradient: Double): Unit = args.foreach(_.backward(gradient))
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}

class LossSum(override val args: Loss*) extends DoubleSum(args:_*) with Loss {
  def backward(): Unit = args.foreach(_.backward())
}



/**
  * Problem 2
  */

/**
  * A block representing a vector parameter
  *
  * @param dim dimension of the vector
  * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
  */
case class VectorParam(dim: Int, clip: Double = 10.0) extends ParamBlock[Vector] with GaussianDefaultInitialization {
  var param: Vector = initialize(defaultInitialization) //todo: initialize using default initialization
  val gradParam: Vector = DenseVector.zeros[Double](dim) //todo: initialize with zeros
  /**
    * @return the current value of the vector parameter and caches it into output
    */
  def forward(): Vector = {
    output=param
    output
  }
  /**
    * Accumulates the gradient in gradParam
    *
    * @param gradient an upstream gradient
    */
  def backward(gradient: Vector): Unit = gradParam :+= gradient
  /**
    * Resets gradParam to zero
    */
  def resetGradient(): Unit = gradParam :*= 0.0
  /**
    * Updates param using the accumulated gradient. Clips the gradient to the interval (-clip, clip) before the update
    *
    * @param learningRate learning rate used for the update
    */
  def update(learningRate: Double): Unit = {
    param :-= (breeze.linalg.clip(gradParam, -clip, clip) * learningRate) //in-place
    resetGradient()
  }
  /**
    * Initializes the parameter randomly using a sampling function
    *
    * @param dist sampling function
    * @return the random parameter vector
    */
  def initialize(dist: () => Double): Vector = {
    param = randVec(dim, dist)
    param
  }
}

/**
  * A block representing the sum of vectors
  *
  * @param args a sequence of blocks that evaluate to vectors
  */
case class Mean(args: Seq[Block[Vector]]) extends Block[Vector] {
  var l:Double = 0.0
  def forward(): Vector = {
    if(output==null){
      l = args.length
      output = args.map(_.forward()).reduce(_ :+ _)
      output = output.map(o=>o/l)
    }
    output
  }
  def backward(gradient: Vector): Unit = {
    val gs = gradient.copy
    args.foreach(_.backward(gs.map(g=>g/l)))
  }

  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}

/**
  * A block representing the sum of vectors
  *
  * @param args a sequence of blocks that evaluate to vectors
  */
case class Sum(args: Seq[Block[Vector]]) extends Block[Vector] {
  def forward(): Vector = {
    if(output==null)
    output = args.map(_.forward()).reduce(_:+_)
    output
  }
  def backward(gradient: Vector): Unit = args.foreach(_.backward(gradient))
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}



case class PoolingMax(args: Seq[Block[Vector]]) extends Block[Vector] {
  var col = 0
  var row = 0
  var loc:DenseMatrix[Double] = DenseMatrix.zeros[Double](row,col)  //mark the max sampling location

  def forward(): Vector = {
    val ys = DenseMatrix(args.map(_.forward().toArray):_*)
    col = ys.cols
    row = ys.rows
    output = DenseVector(Range(0,col).map(ys(::,_).max):_*)
    val rstr = DenseVector(Range(0,col).map(ys(::,_).argmax):_*)
    loc = DenseMatrix.zeros[Double](row,col)
    for(i <- 0 until col){
      loc(rstr(i),i) = 1
    }
    output
  }
  def backward(gradient: Vector): Unit = {
    for(i <- 0 until row){
      args(i).backward(gradient :* loc(i,::).t)
    }
  }
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
}

/**
  * A block representing the sub of vectors
  *
  * @param arg1 a block that evaluate to vectors
  * @param arg2 a block that evaluate to vectors
  */
case class Sub(arg1: Block[Vector],arg2: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = {
    if(output==null)
    output = arg1.forward() :- arg2.forward()
    output
  }
  def backward(gradient: Vector): Unit = {
    arg1.backward(gradient)
    arg2.backward(gradient.map(g => -1*g))
  }
  def update(learningRate: Double): Unit ={
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
  * A block representing the dot product between two vectors
  *
  * @param arg1 left block that evaluates to a vector
  * @param arg2 right block that evaluates to a vector
  */
case class Dot(arg1: Block[Vector], arg2: Block[Vector]) extends Block[Double] {

  def forward(): Double = {
    if(output==null)
      output=arg1.forward() dot arg2.forward()
    output}
  def backward(gradient: Double): Unit = {
    arg1.backward(arg2.output :* gradient)
    arg2.backward(arg1.output :* gradient)
  }
  def update(learningRate: Double): Unit = {
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}


case class Scale(arg1: Block[Vector], arg2: Block[Vector]) extends Block[Vector] {
//    var count = 0
  def forward(): Vector = {
    if(output == null)
      output=arg1.forward().copy :* arg2.forward().copy
//    count+=1
//    println(this.hashCode().toString  + "," + output + ","+ count)
    output
  }
  def backward(gradient: Vector): Unit = {
    arg1.backward(gradient)
    arg2.backward(gradient)
  }
  def update(learningRate: Double): Unit = {
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
  * A block representing the sigmoid of a scalar value
  *
  * @param arg a block that evaluates to a double
  */
case class Sigmoid(arg: Block[Double]) extends Block[Double] {
  def forward(): Double = {
    if(output==null)
    output = sigmoid(arg.forward())
    output
  }
  def backward(gradient: Double): Unit = arg.backward(gradient*output*(1-output))
  def update(learningRate: Double): Unit = arg.update(learningRate)
}

/**
  * A block representing the sigmoid of a scalar value
  *
  * @param arg a block that evaluates to a double
  */
case class SigmoidElementWise(arg: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = {
    if(output==null)
    output = sigmoid(arg.forward())
    output
  }
  def backward(gradient: Vector): Unit = {
    val os = output.copy
    arg.backward(gradient:*os.map(o=>o*(1-o)))
  }
  def update(learningRate: Double): Unit = arg.update(learningRate)
}

//ReLU
case class ReLUElementWise(arg: Block[Vector]) extends Block[Vector] {

  def forward(): Vector = {
    if(output==null)
      output = arg.forward().map(e => {if(e>0) e else 0.0})
    output
  }
  def backward(gradient: Vector): Unit = {
    val os = output.copy
    arg.backward(gradient :* os.map(o=>{
      if(o>0) 1.0 else 0.0
    }))
  }
  def update(learningRate: Double): Unit = arg.update(learningRate)
}

/**
  * A block representing the negative log-likelihood loss
  *   y ⇤ log (f✓ (x))   (1   y) ⇤ log (1   f✓ (x))
  *
  * @param arg a block evaluating to a scalar value
  * @param target the target value (1.0 positive sentiment, 0.0 negative sentiment)
  */
case class NegativeLogLikelihoodLoss(arg: Block[Double], target: Double) extends Loss {
  def forward(): Double = {
    if(output==null)
      output = -1*target*log(arg.forward())-(1-target)*log(1-arg.forward())
    output
  }
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = arg.backward((-1*target)/(arg.output)+(1-target)/(1-arg.output))
  def update(learningRate: Double): Unit = arg.update(learningRate)
}

/**
  * A block representing the l2 regularization of a vector or matrix
  *
  * @param strength the strength of the regularization (often denoted as lambda)
  * @param args a block evaluating to a vector or matrix
  * @tparam P type of the input block (we assume this is Block[Vector] or Block[Matrix]
  */
case class L2Regularization[P](strength: Double, args: Block[P]*) extends Loss {
  def forward(): Double = {
    /**
      * Calculates the loss individually for every vector/matrix parameter in args
      */
    val losses = args.map(arg => {
      val in = arg.forward()
      in match {
        case v: Vector => pow(norm(v,2),2)
        case w: Matrix => pow(norm(w.toDenseVector,2),2)
      }
    })
    output = 0.5*strength*losses.sum //sums the losses up
    output
  }
  def update(learningRate: Double): Unit = args.foreach(_.update(learningRate))
  //loss functions are root nodes so they don't have upstream gradients
  def backward(gradient: Double): Unit = backward()
  def backward(): Unit = args.foreach(x => x.backward((x.output match {
    case v: Vector => v:*strength
    case w: Matrix => w:*strength
  }).asInstanceOf[P]))
}



/**
  * Problem 3
  */

/**
  * A block representing a matrix parameter
  *
  * @param dim1 first dimension of the matrix
  * @param dim2 second dimension of the matrix
  * @param clip defines range in which gradients are clipped, i.e., (-clip, clip)
  */
case class MatrixParam(dim1: Int, dim2: Int, clip: Double = 10.0) extends ParamBlock[Matrix] with GaussianDefaultInitialization {
  var param: Matrix = initialize(defaultInitialization)
  val gradParam: Matrix = DenseMatrix.zeros[Double](dim1,dim2) //randMat(dim1,dim2,()=>0.0)
  def forward(): Matrix = {output=param;output}
  def backward(gradient: Matrix): Unit = {gradParam:+=gradient}
  def resetGradient(): Unit = {gradParam:*=0.0}
  def update(learningRate: Double): Unit = {
    param :-= (breeze.linalg.clip(gradParam, -clip, clip) * learningRate) //in-place
    resetGradient()
  }
  def initialize(dist: () => Double): Matrix = {
    param = randMat(dim1, dim2, dist)
    param
  }
}

/**
  * A block representing matrix-vector multiplication
  *
  * @param arg1 the left block evaluating to a matrix
  * @param arg2 the right block evaluation to a vector
  */
case class Mul(arg1: Block[Matrix], arg2: Block[Vector]) extends Block[Vector] {
  def forward(): Vector = {
    if(output==null)
      output = arg1.forward() * arg2.forward()
    output
  }
  def backward(gradient: Vector): Unit = {
    arg2.backward(arg1.output.t * gradient)
    arg1.backward(outer(gradient,arg2.output))
  }
  def update(learningRate: Double): Unit = {
    arg1.update(learningRate)
    arg2.update(learningRate)
  }
}

/**
  * A block rerpesenting the element-wise application of the tanh function to a vector
  *
  * @param arg a block evaluating to a vector
  */
case class Tanh(arg: Block[Vector]) extends Block[Vector] {

  def forward(): Vector = {
    if(output==null)
      output= tanh(arg.forward())
    output
  }
  def backward(gradient: Vector): Unit = {
    val os = output.copy
    arg.backward(gradient :* os.map(a=>1-pow(a,2)))
  }

  def update(learningRate: Double): Unit = arg.update(learningRate)
}



/**
  * Problem 4
  */

/**
  * A potentially useful block for training a better model (https://en.wikipedia.org/wiki/Dropout_(neural_networks))
  *
  * @param arg a block evaluating to a vector whose components we want to drop
  */
case class Dropout(arg: Block[Vector]) extends Block[Vector] {
  val prob:Double = randomDouble()
  def forward(): Vector = {
    output = arg.forward()
    output
  }
  def update(learningRate: Double): Unit = arg.update(learningRate)
  def backward(gradient: Vector): Unit = {
    arg.backward(gradient)
  }
}

/**
  * ... be free, be creative :)
  */