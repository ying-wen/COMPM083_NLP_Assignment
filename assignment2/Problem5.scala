package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 30/10/2015.
  * @author: yingwen
 */

object Problem5{


  def main (args: Array[String]) {
    println("Joint Extraction")

    val train_dir = "./data/assignment2/bionlp/train"
    val test_dir = "./data/assignment2/bionlp/test"

    // load train and dev data
    // read the specification of the method to load more/less data for debugging speedup
    val (trainDocs, devDocs) = BioNLP.getTrainDevDocuments(train_dir,0.8,500)
    // load test
    val testDocs = BioNLP.getTestDocuments(test_dir)
    // make tuples (Candidate,Gold)
    def preprocess(candidates: Seq[Candidate]) = candidates.map(e => e -> (e.gold,e.arguments.map(_.gold)))

    // ================= Joint Classification =================
    // get candidates and make tuples with gold
    // read the specifications of the method for different subsampling thresholds
    // no subsampling for dev/test!
    def getJointCandidates(docs: Seq[Document]) = docs.flatMap(_.jointCandidates(0.02,0.4))
    def getTestJointCandidates(docs: Seq[Document]) = docs.flatMap(_.jointCandidates())
    val jointTrain = preprocess(getJointCandidates(trainDocs))
    val jointDev = preprocess(getTestJointCandidates(devDocs))
    val jointTest = preprocess(getTestJointCandidates(testDocs))

    // show statistics for counts of true labels, useful for deciding on subsampling
    println("True label counts (trigger - train):")
    println(jointTrain.unzip._2.unzip._1.groupBy(x=>x).mapValues(_.length))
    println("True label counts (trigger - dev):")
    println(jointDev.unzip._2.unzip._1.groupBy(x=>x).mapValues(_.length))
    println("True label counts (argument - train):")
    println(jointTrain.unzip._2.unzip._2.flatten.groupBy(x=>x).mapValues(_.length))
    println("True label counts (argument - dev):")
    println(jointDev.unzip._2.unzip._2.flatten.groupBy(x=>x).mapValues(_.length))


    // get label sets
    val triggerLabels = jointTrain.map(_._2._1).toSet
    val argumentLabels = jointTrain.flatMap(_._2._2).toSet

    // define model
    //TODO: change the features function to explore different types of features
    //TODO: experiment with the unconstrained and constrained (you need to implement the inner search) models


//    val jointModel = JointUnconstrainedClassifier(triggerLabels,argumentLabels,Features.p5TriggerFeatures,Features.p5ArgumentFeatures)
    val jointModel = JointConstrainedClassifier(triggerLabels,argumentLabels,Features.p5TriggerFeatures,Features.p5ArgumentFeatures)

    // use training algorithm to get weights of model
    val jointWeights = PrecompiledTrainers.trainPerceptron(jointTrain,jointModel.feat,jointModel.predict,10)

    // code for testing the performance between two joint models
//    val jointModel1 = JointUnconstrainedClassifier(triggerLabels,argumentLabels,Features.p5TriggerFeatures,Features.p5ArgumentFeatures)
//    val jointModel2 = JointConstrainedClassifier(triggerLabels,argumentLabels,Features.p5TriggerFeatures,Features.p5ArgumentFeatures)
//
//
//    var avg1 = 0.0
//    var avg2 = 0.0
//    for(i<- 1 to 10){
//      val t0 = System.currentTimeMillis()
//      PrecompiledTrainers.trainPerceptron(jointTrain,jointModel1.feat,jointModel1.predict,10)
//      val t1 = System.currentTimeMillis()
//      PrecompiledTrainers.trainPerceptron(jointTrain,jointModel2.feat,jointModel2.predict,10)
//      val t2 = System.currentTimeMillis()
//      println(i+" time(s) --- uncon: " + (t1-t0) + ", con: " + (t2-t1))
//      avg1 += (t1-t0)
//      avg2 += (t2-t1)
//    }
//    println(" \nAverage time for 10 times training:\n --- naive-ave: " + avg1/10 + ", improve-avg: " + avg2/10)

    // get predictions on dev
    val jointDevPred = jointDev.unzip._1.map { case e => jointModel.predict(e,jointWeights) }
    val jointDevGold = jointDev.unzip._2

    // Triggers (dev)
    val triggerDevPred = jointDevPred.unzip._1
    val triggerDevGold = jointDevGold.unzip._1
    val triggerDevEval = Evaluation(triggerDevGold,triggerDevPred,Set("None"))
    println("Evaluation for trigger classification:")
    println(triggerDevEval.confusion)
    println(triggerDevEval.toString)

    // Arguments (dev)
    val argumentDevPred = jointDevPred.unzip._2.flatten
    val argumentDevGold = jointDevGold.unzip._2.flatten
    val argumentDevEval = Evaluation(argumentDevGold,argumentDevPred,Set("None"))
    println("Evaluation for argument classification:")
    println(argumentDevEval.confusion)
    println(argumentDevEval.toString)

    // get predictions on test
    val jointTestPred = jointTest.unzip._1.map { case e => jointModel.predict(e,jointWeights) }
    // Triggers (test)
    val triggerTestPred = jointTestPred.unzip._1
    // write to file
    Evaluation.toFile(triggerTestPred,"./data/assignment2/out/joint_trigger_test.txt")
    // Arguments (test)
    val argumentTestPred = jointTestPred.unzip._2.flatten
    // write to file
    Evaluation.toFile(argumentTestPred,"./data/assignment2/out/joint_argument_test.txt")
  }

}

/**
 * A joint event classifier (both triggers and arguments).
 * It predicts the structured event.
 * It's predict method should only produce the best solution that respects the constraints on the event structure.
 * @param triggerLabels
 * @param argumentLabels
 * @param triggerFeature
 * @param argumentFeature
 */
case class JointConstrainedClassifier(triggerLabels:Set[Label],
                                      argumentLabels:Set[Label],
                                      triggerFeature:(Candidate,Label)=>FeatureVector,
                                      argumentFeature:(Candidate,Label)=>FeatureVector
                                       ) extends JointModel {
  /**
    * Constraint 1: if e=None, all a=None
    * Constraint 2: if e!=None, at least one a=Theme
    * Constraint 3: only e=Regulation can have a=Cause
    * @param x
    * @param weights
    * @return
    */
  def predict(x: Candidate, weights: Weights) = {
    def argmax(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
        val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
        scores.maxBy(_._2)._1
    }

    def argmaxForArg(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
      (scores.maxBy(_._2)._1,scores("Theme"))
    }
    var bestTrigger = "None"


    bestTrigger = argmax(triggerLabels,x,weights,triggerFeature)

    val oneThemelabelSet = Set("Phosphorylation", "Expression", "Gene_expression","Transcription", "Localization", "Protein_catabolism")
    val regulationLabelSet = Set("Regulation","Negative_regulation","Positive_regulation")
    var bestArguments = for (arg<-x.arguments) yield "None"


      if(bestTrigger!="None"){
        var arguments = Seq[(Label,Double)]()
        if(regulationLabelSet.contains(bestTrigger)){
          arguments = for (arg<-x.arguments) yield argmaxForArg(argumentLabels,arg,weights,argumentFeature)

        }else{
          arguments = for (arg<-x.arguments) yield {
            argmaxForArg(argumentLabels -- Set("Cause"),arg,weights,argumentFeature)
          }
        }
        bestArguments = arguments.map(x => x._1)
        val ThemeScore = arguments.map(x => x._2)
        val index :Int = ThemeScore.indexOf(ThemeScore.max)
        bestArguments = bestArguments.updated(index,"Theme")

      }

    (bestTrigger,bestArguments)
  }

}

/**
 * A joint event classifier (both triggers and arguments).
 * It predicts the structured event.
 * It treats triggers and arguments independently, i.e. it ignores any solution constraints.
 * @param triggerLabels
 * @param argumentLabels
 * @param triggerFeature
 * @param argumentFeature
 */
case class JointUnconstrainedClassifier(triggerLabels:Set[Label],
                                        argumentLabels:Set[Label],
                                        triggerFeature:(Candidate,Label)=>FeatureVector,
                                        argumentFeature:(Candidate,Label)=>FeatureVector
                                         ) extends JointModel{
  /**
   * @param x
   * @param weights
   * @return
   */
  def predict(x: Candidate, weights: Weights) = {
    def argmax(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
      scores.maxBy(_._2)._1
    }
    val bestTrigger = argmax(triggerLabels,x,weights,triggerFeature)
    val bestArguments = for (arg<-x.arguments) yield argmax(argumentLabels,arg,weights,argumentFeature)
    (bestTrigger,bestArguments)
  }

}

trait JointModel extends Model[Candidate,StructuredLabels]{
  def triggerFeature:(Candidate,Label)=>FeatureVector
  def argumentFeature:(Candidate,Label)=>FeatureVector
  def feat(x: Candidate, y: StructuredLabels): FeatureVector ={
    val f = new mutable.HashMap[FeatureKey, Double] withDefaultValue 0.0
    addInPlace(triggerFeature(x,y._1),f,1)
    for ((a,label)<- x.arguments zip y._2){
      addInPlace(argumentFeature(a,label),f,1)
    }
    f
  }
}


