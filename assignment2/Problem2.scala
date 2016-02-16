package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 11/11/2015.
  * @author: yingwen
 */
object Problem2 {


  /**
   * Train a linear model using the average perceptron algorithm.
   * @param instances the training instances.
   * @param feat a joint feature function.
   * @param predict a prediction function that maps inputs to outputs using the given weights.
   * @param iterations number of iterations.
   * @param learningRate
   * @tparam X type of input.
   * @tparam Y type of output.
   * @return a linear model trained using the perceptron algorithm.
   */
  def trainAvgPerceptron[X, Y](instances: Seq[(X, Y)],
                               feat: (X, Y) => FeatureVector,
                               predict: (X, Weights) => Y,
                               iterations: Int = 2,
                               learningRate: Double = 1.0): Weights = {
    //TODO implement the averaged perceptron trainer
    val w = mutable.Map[FeatureKey,Double]() withDefaultValue 0.0
    val last = mutable.Map[FeatureKey,Double]() withDefaultValue 0.0
    val avg = mutable.Map[FeatureKey,Double]() withDefaultValue 0.0
    var count = 0
    for(i <- 1 to iterations){
      count = 0
      instances.foreach(instance => {
        val x = instance._1
        val y = instance._2
        val f_x = feat(x,y)
        val best = predict(x,w)
        val f_best = feat(x,best)
        if(y != best){
          update((f_x.keySet ++ f_best.keySet),w,last,avg,count*iterations)
          addInPlace(f_x,w,learningRate)
          addInPlace(f_best,w,-learningRate)
        }
        count += 1
      })
    }
    update(w.keySet,w,last,avg,count*iterations)
    avg
  }

  def update(keys: Iterable[FeatureKey],w:mutable.Map[FeatureKey,Double],last:mutable.Map[FeatureKey,Double],avg:mutable.Map[FeatureKey,Double],count:Int) = {
      keys.foreach(k=>{
        val l = last(k)
        if(count>0){
          avg(k) = (avg(k)*l + w(k)*(count - l))/count
        }
        last(k) = count
      })
  }


  def trainAvgPerceptronUnImprove[X, Y](instances: Seq[(X, Y)],
                                        feat: (X, Y) => FeatureVector,
                                        predict: (X, Weights) => Y,
                                        iterations: Int = 2,
                                        learningRate: Double = 1.0): Weights = {
    //TODO implement the averaged perceptron trainer
    val w = mutable.Map[FeatureKey,Double]() withDefaultValue 0.0
    val avg = mutable.Map[FeatureKey,Double]() withDefaultValue 0.0
    var count = iterations*instances.size
    for(i <- 1 to iterations){
      instances.foreach(instance => {
        val x = instance._1
        val y = instance._2
        val f_x = feat(x,y)
        val guess = predict(x,w)
        val f_g = feat(x,guess)
        if(y != guess){
          addInPlace(f_x,w,learningRate)
          addInPlace(f_g,w,-learningRate)
        }
        addInPlace(w,avg,1)
      })
    }
    avg.map(a=>a._2/count)
    avg
  }


  /**
   * Run this code to evaluate your implementation of your avereaged perceptron algorithm trainer
   * Results should be similar to the precompiled trainer
   * @param args
   */
  def main (args: Array[String] ) {

    val train_dir = "./data/assignment2/bionlp/train"

    // load train and dev data
    // read the specification of the method to load more/less data for debugging speedup
    val (trainDocs, devDocs) = BioNLP.getTrainDevDocuments(train_dir, 0.8, 100)
    // make tuples (Candidate,Gold)
    def preprocess(candidates: Seq[Candidate]) = candidates.map(e => e -> e.gold)

    // ================= Trigger Classification =================

    // get candidates and make tuples with gold
    // read the specifications of the method for different subsampling thresholds
    // no subsampling for dev/test!
    def getTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates(0.02))
    def getTestTriggerCandidates(docs: Seq[Document]) = docs.flatMap(_.triggerCandidates())
    val triggerTrain = preprocess(getTriggerCandidates(trainDocs))
    val triggerDev = preprocess(getTestTriggerCandidates(devDocs))

    // get label set
    val triggerLabels = triggerTrain.map(_._2).toSet

    // define model
    val triggerModel = SimpleClassifier(triggerLabels, defaultTriggerFeatures)

    // code for testing the performance
//    var avg1,avg2,avg3 = 0.0
//    for(i<- 1 to 100){
//      val t0 = System.currentTimeMillis()
//      val w = trainAvgPerceptronUnImprove(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
//      val t1 = System.currentTimeMillis()
//      val myWeights = trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
//      val t2 = System.currentTimeMillis()
//      val precompiledWeights = PrecompiledTrainers.trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
//      val t3 = System.currentTimeMillis()
//      // get predictions on dev
//      val (myPred, gold) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, myWeights), gold) }.unzip
//      val (precompiledPred, _) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, precompiledWeights), gold) }.unzip
//      println(i+" time(s) --- naive-ave: " + (t1-t0) + ", improve-avg: " + (t2-t1) +" ,precom-avg: " +(t3-t2))
//      avg1 += (t1-t0)
//      avg2 += (t2-t1)
//      avg3 += (t3-t2)
//    }
//    println(" \nAverage time for 20 times training:\n --- naive-ave: " + avg1/100 + ", improve-avg: " + avg2/100 +" ,precom-avg: " + avg3/100)


    val myWeights = trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
    val precompiledWeights = PrecompiledTrainers.trainAvgPerceptron(triggerTrain, triggerModel.feat, triggerModel.predict, 1)
    val (myPred, gold) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, myWeights), gold) }.unzip
    val (precompiledPred, _) = triggerDev.map { case (trigger, gold) => (triggerModel.predict(trigger, precompiledWeights), gold) }.unzip

    // evaluate models (dev)
    println("Evaluation - my trainer:")
    println(Evaluation(gold, myPred, Set("None")).toString)
    println("Evaluation - precompiled trainer:")
    println(Evaluation(gold, precompiledPred, Set("None")).toString)
  }

  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }


}
