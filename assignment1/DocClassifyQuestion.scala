package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlpbook.assignment1.Assignment1Util.Instance
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.{UniformLM, LanguageModel}

import scala.collection.mutable


/**
 * @author mbosnjak, yingwen
 */
object DocClassifyQuestion {

  case class Classifier(train: Seq[Instance])  {
    var vocab = mutable.Set("")
    def order = 1
    val eps = 0.02
    val stopTokenSet = Set(",")
    var authorTotalCount:Double = 0.0
    var authorCount = new mutable.HashMap[String,Double]() withDefaultValue 0.0
    val counts = new mutable.HashMap[List[String], Double] withDefaultValue 0.0
    val norm = new mutable.HashMap[List[String], Double] withDefaultValue 0.0

    def dataCleaning(data:String) = {
      val dList = data.split(" ").filter(l=> (!stopTokenSet(l))).toList
      vocab.union(dList.toSet)
      dList
    }

    def trainModel() = {
      train.foreach(instance=>{

        val author = instance.author.get
        val lyrics = dataCleaning(instance.lyrics)
        authorCount(author) += 1.0
        for (i <- order until lyrics.length) {
          val history = lyrics.slice(i - order + 1, i).toList
          val word = lyrics(i)
          counts(author :: word :: history) += 1.0
          norm(author :: history) += 1.0
        }
      })
    }

    def calProb(lyrics:List[String], author:String) = {
      var prob = 0.0
      for (i <- order until lyrics.length) {
        val history = lyrics.slice(i - order + 1, i)
        val word = lyrics(i)
        prob += Math.log((counts(author :: word :: history)+eps)/(norm(author :: history)+eps*vocab.size))
      }
      prob
    }

    def classify(lyrics1:String):Option[String] = {
      val lyrics = dataCleaning(lyrics1)
      val rsts = new mutable.HashMap[String,Double]() withDefaultValue 0.0
      val scoreByModel = authorCount.foreach(a => {
        val probAuthor = Math.log(a._2/train.length)
        rsts(a._1) = (probAuthor + calProb(lyrics,a._1))
      })
      val rst = rsts.maxBy(_._2)._1
      Option(rst)
    }

  }

  def main(args: Array[String]): Unit = {
    // load the datasets

    val train = Assignment1Util.loadDataset(new File("data/assignment1/p3/p3_train.txt"))
    val dev = Assignment1Util.loadDataset(new File("data/assignment1/p3/p3_dev.txt"))
    val test = Assignment1Util.loadDataset(new File("data/assignment1/p3/p3_test.txt"))


    val classifier = Classifier(train)
    classifier.trainModel()
//    dev.foreach(d=>{
//      println(d.author.get + "," +classifier.classify(d.lyrics).get)
//    })
//    // TODO given an instance, how would you classify it
    def classify(instance: Instance) = {
      classifier.classify(instance.lyrics)
    }
//
//
//
// execute your classifier
    val predictionsTrain = train.map(i => Instance(i.lyrics, i.author, classify(i)))

    // accurately predicted instances
    val accuratelyPredictedTrain = predictionsTrain.map(i => i.author.get == i.prediction.get).count(_ == true)

    // total number of instances
    val totalInstancesTrain = predictionsTrain.length
    //    println(accuratelyPredictedDEV+","+totalInstances)
    // evaluate accuracy
    val accuracyTrain = 1.0 * accuratelyPredictedTrain / totalInstancesTrain
    println("classification accuracy for train data: " + accuracyTrain)

    // execute your classifier
    val predictionsDEV = dev.map(i => Instance(i.lyrics, i.author, classify(i)))

    // accurately predicted instances
    val accuratelyPredictedDEV = predictionsDEV.map(i => i.author.get == i.prediction.get).count(_ == true)

    // total number of instances
    val totalInstancesDEV = predictionsDEV.length
//    println(accuratelyPredictedDEV+","+totalInstances)
    // evaluate accuracy
    val accuracy = 1.0 * accuratelyPredictedDEV / totalInstancesDEV
    println("classification accuracy for dev data: " + accuracy)

  }



}
