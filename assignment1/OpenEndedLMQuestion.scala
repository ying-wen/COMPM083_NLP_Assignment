package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels._

import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.io.Source

/**
 * @author riedel, yingwen
 */
object OpenEndedLMQuestion {

  /**
   * An LM that performs well on the dev set.
   */
  case class MyReallyGooDLM(vocab: Set[String],trainFile: File,lapDiscount: LaplaceLMWithDiscounts) extends LanguageModel {
    def order = 20
    def orderForGram = 1
    def eps = 10

    val countMap:Map[Int, Double] = {
      val content = Source.fromFile(trainFile).getLines().mkString(" ")
      val bars = content.split("\\[/BAR\\] \\[BAR\\]")
      val distanceArray = new  ArrayBuffer[Int]()
      bars.foreach(bar=>{
        if (bar==" ") {
          distanceArray += 0
        } else {
          val sentence = bar.trim.split(" ")
          val l = sentence.length
//          for (i <- orderForGram until sentence.length) {
//            val history = sentence.slice(i - orderForGram + 1, i).toList
//            val word = sentence(i)
//            counts(word :: history) += 1.0
//            norm(history) += 1.0
//          }
          if(l<=18)
            distanceArray += l
          else
            distanceArray += 19
        }
      })
      distanceArray.groupBy((l:Int)=>l).mapValues(_.length)
    }
    val countTotalMap:HashMap[Int, Double] = {
      val tmap = new HashMap[Int, Double]
      for(i <- 0 to 19){
        var sum = 0.0
        for(j <- i to 19){
          sum = sum + countMap(j)
        }
        tmap(i) = sum
      }
      tmap
    }
//    val counts = new mutable.HashMap[List[String], Double] withDefaultValue 0.0
//    val norm = new mutable.HashMap[List[String], Double] withDefaultValue 0.0

//    for (i <- orderForGram until train.length) {
//      val history = train.slice(i - orderForGram + 1, i).toList
//      val word = train(i)
//      counts(word :: history) += 1.0
//      norm(history) += 1.0
//    }
//    println(counts)
//    println(norm)

    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*):Double = {
      var dist = order - 1
      var prob = 0.0
      if(history(history.length - 1)=="[/BAR]"){
        if(word == "[BAR]") {
          prob = 1.0

        } else {
          prob = 0.0
        }
      } else {
        for (i <- 0 until history.length - 1) {
          if (history(i) == "[BAR]") {
            dist = order - 2 - i
          }
        }
        val p = (countMap(dist) + eps) / (countTotalMap(dist) + eps*order)
        if (word == "[/BAR]") {
          prob = p
        } else if (word == "[BAR]") {
          prob = 0.0
        }
        else {
          val nHistory = history.toList.filter(d=>(d!="[BAR]" && d!="[/BAR]"))
          prob = (1-p)*lapDiscount.probability(word,nHistory.takeRight(lapDiscount.order - 1).mkString)
        }
      }

      prob
    }
  }

  def main(args: Array[String]) {
    //The training file we provide you
    val trainFile = new File("data/assignment1/p2/p2_train.txt")

    //the dev file we provide you.
    val devFile = new File("data/assignment1/p2/p2_dev.txt")

    //the training sequence of words
    val train = Assignment1Util.loadWords(trainFile).toBuffer

    //the dev sequence of words
    val dev = Assignment1Util.loadWords(devFile).toBuffer

    //the vocabulary. Contains the training words and the OOV symbol (as the dev set has been preprocessed by
    //replacing words not in the training set with OOV).
    val vocab = train.toSet + Util.OOV
    val trainOOV = Util.injectOOVs(Util.OOV,train.toSeq)
//    val uniGramLM = NGramLM(trainOOV, 1)
    //trainOOV.to.filter(d => (d!="[BAR]" && d!="[/BAR]"))
//    println(trainOOV.getClass)
    val nuigram = NGramLM(trainOOV.filter(d => (d!="[BAR]" && d!="[/BAR]")),1)
    val lapDiscount = LaplaceLMWithDiscounts(nuigram,3)
    //TODO: Improve the MyBarAwareLM implementation
    val lm = MyReallyGooDLM(vocab,trainFile,lapDiscount)

    //This calculates the perplexity of the
    val pp = LanguageModel.perplexity(lm, dev)

    println("Best pp :" + pp)
    println(LanguageModel.sample(lm,List("[BAR]"),1000).mkString(" "))
  }

}
