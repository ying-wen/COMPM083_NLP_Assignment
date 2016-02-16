package uk.ac.ucl.cs.mr.statnlpbook.assignment3
import breeze.linalg._
import breeze.numerics._
import java.io.PrintWriter
import scala.io
/**
 * @author rockt
 */
object Main extends App {
  /**
   * Example training of a model
   *
   * Problems 2/3/4: perform a grid search over the parameters below
   */
  def loadPreTrained(): Unit ={
    println("Pre-trained loading")
    val f = io.Source.fromFile("./data/assignment3/glove.twitter.27B.25d.txt", "utf-8")
    f.getLines().foreach(line=>{
      val ws = line.split(" ")
      val v = ws.slice(1,ws.size).map(w=>w.toDouble*0.1)
      LookupTable.addTrainableWordVector(ws(0),DenseVector(v))
    })
    println("Done")
  }


  //  Best params for SumOfWordVectorsModel
  //  val learningRate = 0.01
  //  val vectorRegularizationStrength = 0.1
  //  val matrixRegularizationStrength = 0.01
  //  val wordDim = 27
  //  val hiddenDim = 25


  //  Best params for RecurrentNeuralNetworkModel
  //  val learningRate = 0.01
  //  val vectorRegularizationStrength = 0.02
  //  val matrixRegularizationStrength = 0.02
  //  val wordDim = 20
  //  val hiddenDim = 10


  // best params for RCNN
  val learningRate = 0.01
  val vectorRegularizationStrength = 0.001
  val matrixRegularizationStrength = 0.001
  val wordDim = 25
  val hiddenDim = 25

  val ifLoadPreTrained = true

  val trainSetName = "train"
  val validationSetName = "dev"

  if(ifLoadPreTrained)
    loadPreTrained()

//  val model: Model = new SumOfWordVectorsModel(wordDim, vectorRegularizationStrength)
//  val model: Model = new RecurrentNeuralNetworkModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)
//  val model: Model = new GRUModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)
  val model: Model = new RCNNModel(wordDim, hiddenDim, vectorRegularizationStrength, matrixRegularizationStrength)

  def epochHook(iter: Int, accLoss: Double): Unit = {
    println("Epoch %4d\tLoss %8.4f\tTrain Acc %4.2f\tDev Acc %4.2f".format(
      iter, accLoss, 100 * Evaluator(model, trainSetName), 100*Evaluator(model, validationSetName)))
  }

  StochasticGradientDescentLearner(model, trainSetName, 150, learningRate, epochHook)



  /**
   * Comment this in if you want to look at trained parameters
   */
  val out = new PrintWriter("./data/assignment3/wordVec.csv")
  for ((paramName, paramBlock) <- model.vectorParams) {
    val t = if(model.scoreSentence(paramBlock).forward() >= 0.5)  1 else 0
    out.println("\""+paramName + "\","+ t+","+ paramBlock.param.toArray.mkString(","))
  }
  out.close()
  for ((paramName, paramBlock) <- model.matrixParams) {
    println(s"$paramName:\n${paramBlock.param}\n")
  }

}