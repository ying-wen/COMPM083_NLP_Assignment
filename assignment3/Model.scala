package uk.ac.ucl.cs.mr.statnlpbook.assignment3

import breeze.linalg.{DenseVector, DenseMatrix}
import scala.collection.mutable
import scala.io

/**
  * @author rockt, yingwen
  */
trait Model {
  /**
    * Stores all vector parameters
    */
  val vectorParams = new mutable.HashMap[String, VectorParam]()
  /**
    * Stores all matrix parameters
    */
  val matrixParams = new mutable.HashMap[String, MatrixParam]()
  /**
    * Maps a word to its trainable or fixed vector representation
    *
    * @param word the input word represented as string
    * @return a block that evaluates to a vector/embedding for that word
    */
  def wordToVector(word: String): Block[Vector]
  /**
    * Composes a sequence of word vectors to a sentence vectors
    *
    * @param words a sequence of blocks that evaluate to word vectors
    * @return a block evaluating to a sentence vector
    */
  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector]
  /**
    * Calculates the score of a sentence based on the vector representation of that sentence
    *
    * @param sentence a block evaluating to a sentence vector
    * @return a block evaluating to the score between 0.0 and 1.0 of that sentence (1.0 positive sentiment, 0.0 negative sentiment)
    */
  def scoreSentence(sentence: Block[Vector]): Block[Double]
  /**
    * Predicts whether a sentence is of positive or negative sentiment (true: positive, false: negative)
    *
    * @param sentence a tweet as a sequence of words
    * @param threshold the value above which we predict positive sentiment
    * @return whether the sentence is of positive sentiment
    */
  def predict(sentence: Seq[String])(implicit threshold: Double = 0.5): Boolean = {
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    scoreSentence(sentenceVector).forward() >= threshold
  }
  /**
    * Defines the training loss
    *
    * @param sentence a tweet as a sequence of words
    * @param target the gold label of the tweet (true: positive sentiement, false: negative sentiment)
    * @return a block evaluating to the negative log-likelihod plus a regularization term
    */
  def loss(sentence: Seq[String], target: Boolean): Loss = {
    val targetScore = if (target) 1.0 else 0.0
    val wordVectors = sentence.map(wordToVector)
    val sentenceVector = wordVectorsToSentenceVector(wordVectors)
    val score = scoreSentence(sentenceVector)
    new LossSum(NegativeLogLikelihoodLoss(score, targetScore), regularizer(wordVectors))
  }
  /**
    * Regularizes the parameters of the model for a given input example
    *
    * @param words a sequence of blocks evaluating to word vectors
    * @return a block representing the regularization loss on the parameters of the model
    */
  def regularizer(words: Seq[Block[Vector]]): Loss
}


/**
  * Problem 2
  * A sum of word vectors model
  *
  * @param embeddingSize dimension of the word vectors used in this model
  * @param regularizationStrength strength of the regularization on the word vectors and global parameter vector w
  */
class SumOfWordVectorsModel(embeddingSize: Int, regularizationStrength: Double = 0.0) extends Model {
  /**
    * We use a lookup table to keep track of the word representations
    */

  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  /**
    * We are also going to need another global vector parameter
    */
  vectorParams += "param_w" -> VectorParam(embeddingSize)

  def wordToVector(word: String): Block[Vector] = {
    if(!vectorParams.contains(word))
      vectorParams += word -> VectorParam(embeddingSize)
    vectorParams(word)
  }
  val prob = 0.2
  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    Sum(words)
  }


  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid(Dot(sentence,vectorParams("param_w")))

  def regularizer(words: Seq[Block[Vector]]): Loss = L2Regularization(regularizationStrength, words :+ vectorParams("param_w"):_* )
  }


/**
  * Problem 3
  * A recurrent neural network model
  *
  * @param embeddingSize dimension of the word vectors used in this model
  * @param hiddenSize dimension of the hidden state vector used in this model
  * @param vectorRegularizationStrength strength of the regularization on the word vectors and global parameter vector w
  * @param matrixRegularizationStrength strength of the regularization of the transition matrices used in this model
  */
class RecurrentNeuralNetworkModel(embeddingSize: Int, hiddenSize: Int,
                                  vectorRegularizationStrength: Double = 0.0,
                                  matrixRegularizationStrength: Double = 0.0) extends Model {
  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  val prob =  0.2

  LookupTable.addTrainableWordVector("param_w",randVec(hiddenSize,()=>random.nextGaussian() * 0.1))
  LookupTable.addTrainableWordVector("param_h0",randVec(hiddenSize,()=>random.nextGaussian() * 0.1))
  LookupTable.addTrainableWordVector("param_b",randVec(hiddenSize,()=>random.nextGaussian() * 0.1))


  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
  matrixParams += "param_Wx" -> {
    val ww = MatrixParam(hiddenSize, embeddingSize)
    val w = randMat(hiddenSize, embeddingSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wh" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }

  def wordToVector(word: String): Block[Vector] = {
    if(!vectorParams.contains(word))
    //      vectorParams += word -> VectorParam(embeddingSize)
      LookupTable.addTrainableWordVector(word,randVec(embeddingSize,()=>random.nextGaussian() * 0.1))
    vectorParams(word)
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    words.foldLeft(vectorParams("param_h0"):Block[Vector]) { (h:Block[Vector],x:Block[Vector]) =>
      Tanh(Sum(mutable.Seq(Mul(matrixParams("param_Wh"),h), Mul(matrixParams("param_Wx"),x), vectorParams("param_b"))))
    }
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid(Dot(sentence,vectorParams("param_w")))

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, words:+vectorParams("param_w"):+vectorParams("param_h0"):+vectorParams("param_b"):_*),
      L2Regularization(matrixRegularizationStrength, matrixParams("param_Wh"),matrixParams("param_Wx"))
    )
}


/**
  * Problem 5
  * A recurrent neural network model (GRU)
  *
  * @param embeddingSize dimension of the word vectors used in this model
  * @param hiddenSize dimension of the hidden state vector used in this model
  * @param vectorRegularizationStrength strength of the regularization on the word vectors and global parameter vector w
  * @param matrixRegularizationStrength strength of the regularization of the transition matrices used in this model
  */
class GRUModel(embeddingSize: Int, hiddenSize: Int,
               vectorRegularizationStrength: Double = 0.0,
               matrixRegularizationStrength: Double = 0.0) extends Model {

  val prob = 0.2

  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_h0" -> VectorParam(hiddenSize)
  vectorParams += "param_br" -> VectorParam(hiddenSize)
  vectorParams += "param_bz" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)


  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()
  matrixParams += "param_Wrx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wrh" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wzx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wzh" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wh" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }

  def wordToVector(word: String): Block[Vector] = {
    if(!vectorParams.contains(word))
      vectorParams += word -> VectorParam(embeddingSize)
    vectorParams(word)
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    //    val words_f = words.map(Dropout(_)).filter(_.prob>prob)
    var h:Block[Vector] = vectorParams("param_h0")
    var hs = mutable.Seq[Block[Vector]](h)
    words.foreach(x => {
        val r = SigmoidElementWise(Sum(mutable.Seq(Mul(matrixParams("param_Wrx"), x), Mul(matrixParams("param_Wrh"), h),vectorParams("param_br"))))
        val z = SigmoidElementWise(Sum(mutable.Seq(Mul(matrixParams("param_Wzx"), x), Mul(matrixParams("param_Wzh"), h),vectorParams("param_bz"))))
        val _hr = Mul(matrixParams("param_Wh"), Scale(r,h))
        val _hx = Mul(matrixParams("param_Wx"), x)
        val h_ = Tanh(Sum(mutable.Seq(_hr,_hx,vectorParams("param_b"))))
        val one = DenseVector.ones[Double](hiddenSize)
        h = new Sum(mutable.Seq(Scale(h, z), Scale(Sub(one, z), h_)))
        hs = hs :+ h
      })
      Mean(hs)
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid(Dot(sentence,vectorParams("param_w")))

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      //      L2Regularization(vectorRegularizationStrength, words :+vectorParams("param_w"):+vectorParams("param_h0"):_*),
      L2Regularization(vectorRegularizationStrength, words :+vectorParams("param_w"):+vectorParams("param_h0"):+vectorParams("param_br"):+vectorParams("param_bz"):+vectorParams("param_b"):_*),
      //      L2Regularization(vectorRegularizationStrength, vectorParams("param_w"),vectorParams("param_h0")),
      L2Regularization(matrixRegularizationStrength, matrixParams("param_Wh"),matrixParams("param_Wx"),matrixParams("param_Wrh"),matrixParams("param_Wrx"),matrixParams("param_Wzx"),matrixParams("param_Wzh"))
    )
}


/**
  * Problem 5
  * A recurrent neural network model (GRU)
  *
  * @param embeddingSize dimension of the word vectors used in this model
  * @param hiddenSize dimension of the hidden state vector used in this model
  * @param vectorRegularizationStrength strength of the regularization on the word vectors and global parameter vector w
  * @param matrixRegularizationStrength strength of the regularization of the transition matrices used in this model
  */
class RCNNModel(embeddingSize: Int, hiddenSize: Int,
               vectorRegularizationStrength: Double = 0.0,
               matrixRegularizationStrength: Double = 0.0) extends Model {

  val prob = 0.2    //dropout probability

  override val vectorParams: mutable.HashMap[String, VectorParam] =
    LookupTable.trainableWordVectors
  vectorParams += "param_w" -> VectorParam(hiddenSize)
  vectorParams += "param_cr0" -> VectorParam(hiddenSize)
  vectorParams += "param_cl0" -> VectorParam(hiddenSize)
  vectorParams += "param_br" -> VectorParam(hiddenSize)
  vectorParams += "param_bl" -> VectorParam(hiddenSize)
  vectorParams += "param_b" -> VectorParam(hiddenSize)


  override val matrixParams: mutable.HashMap[String, MatrixParam] =
    new mutable.HashMap[String, MatrixParam]()

  matrixParams += "param_Wrx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wrc" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wlx" -> MatrixParam(hiddenSize, embeddingSize)
  matrixParams += "param_Wlc" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wycr" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wycl" -> {
    val ww = MatrixParam(hiddenSize, hiddenSize)
    val w =  DenseMatrix.eye[Double](hiddenSize) //andMat(hiddenSize, hiddenSize,()=>random.nextGaussian() * 0.1)
    ww.set(w)
    ww
  }
  matrixParams += "param_Wx" -> MatrixParam(hiddenSize, embeddingSize)
  println("vectorParams size:"+ vectorParams.size)
  def wordToVector(word: String): Block[Vector] = {
    if(!vectorParams.contains(word))
      vectorParams += word -> VectorParam(embeddingSize)
    vectorParams(word)
  }

  def wordVectorsToSentenceVector(words: Seq[Block[Vector]]): Block[Vector] = {
    //    val words_f = words.map(Dropout(_)).filter(_.prob>prob)
//    var h:Block[Vector] = vectorParams("param_h0")
    val ws = words
//    var ws:Seq[Block[Vector]] = words.map(Dropout(_)).filter(_.prob>prob)
//    if(ws.length == 0)
//      ws = words
    val l = ws.length
    var crs = List[Block[Vector]]()
    var cls = List[Block[Vector]]()
    var ys = List[Block[Vector]]()
//    val hs = mutable.Seq(h)
    var cr:Block[Vector] = vectorParams("param_cr0")
    var cl:Block[Vector] = vectorParams("param_cl0")
    var y:Block[Vector] = null
    for(i <- 0 until l){
      cl = ReLUElementWise(Sum(mutable.Seq(Mul(matrixParams("param_Wlx"), ws(i)), Mul(matrixParams("param_Wlc"), cl),vectorParams("param_bl"))))
      cls = cls :+ cl
    }
    for(i <- 0 until l){
      cr = ReLUElementWise(Sum(mutable.Seq(Mul(matrixParams("param_Wrx"), ws(l-i-1)), Mul(matrixParams("param_Wrc"), cr),vectorParams("param_br"))))
      crs = crs :+ cr
    }
    crs = crs.reverse

    for(i <- 0 until l){
      y = Tanh(Sum(mutable.Seq(Mul(matrixParams("param_Wx"),ws(i)),Mul(matrixParams("param_Wycr"),crs(i)),Mul(matrixParams("param_Wycl"),cls(i)),vectorParams("param_b"))))
      ys = ys :+ y
    }
    PoolingMax(ys)
  }

  def scoreSentence(sentence: Block[Vector]): Block[Double] = Sigmoid(Dot(sentence,vectorParams("param_w")))

  def regularizer(words: Seq[Block[Vector]]): Loss =
    new LossSum(
      L2Regularization(vectorRegularizationStrength, words :+vectorParams("param_w"):+vectorParams("param_cr0"):+vectorParams("param_cl0"):+vectorParams("param_br"):+vectorParams("param_bl"):+vectorParams("param_b"):_*),
      L2Regularization(matrixRegularizationStrength, matrixParams("param_Wrx"),matrixParams("param_Wrc"),matrixParams("param_Wlx"),matrixParams("param_Wrx"),matrixParams("param_Wrc"),matrixParams("param_Wycl"),matrixParams("param_Wycr"),matrixParams("param_Wx"))
    )
}
