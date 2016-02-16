package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels._

/**
 * @author riedel, yingwen
 */
object BarQuestion {

  /**
   * An LM that assigns increasing probability to the [/BAR] token the further the last [/BAR] token is away,
   * and uniform probability for all other tokens. How to choose the function that maps [/BAR] distance to a probability
   * is left to you, but you should guide your choice by the goal of minimizing perplexity.
   * @param vocab the vocabulary.
   */
  case class MyBarAwareLM(vocab: Set[String],a: Double) extends LanguageModel {
    def order = 20
    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*):Double = {
      var dist = order - 1
      for(i <- 0 until history.length) {
          if(history(i)=="[BAR]") {
            dist = order - 2 - i
          }
        }
        if(word == "[/BAR]") {
            a*dist + a
        } else {
            (1-a-a*dist)/(vocab.size-1)
        }
    }
  }

  case class PerDistanceLM(train:IndexedSeq[String], order:Int) extends LanguageModel{
    var dis = 19
    val vocab = train.toSet
    val counts = new HashMap[Int, Double] withDefaultValue 0.0
    val norm = new HashMap[Int, Double] withDefaultValue 0.0

    for (i <- order until train.length) {
      val history = train.slice(i - order + 1, i).toList
      val word = train(i)
      var barIndex = -1
      if(history.contains("[BAR]"))
        barIndex = history.lastIndexOf("[BAR]")
      val dis = order - 1 -barIndex -1
      if (word == "[/BAR]") counts(dis) += 1.0
      norm(dis) += 1.0
    }
    //for(j<-0 until 20)
    //println(counts(j),norm(j))


    def probability(word:String, history:String*)={
      // val index = history.lastIndexOf("[/BAR]")
      var barIndex = -1
      if(history.contains("[BAR]"))
        barIndex = history.lastIndexOf("[BAR]")
      val dis = order - 1 -barIndex -1
      val prob = counts(dis) / norm(dis)
      println(dis,prob)
      if(word == "[/BAR]") prob
      else (1-prob)/ (vocab.size-1)
    }

  }

  case class BarInterpolatedLM(main:LanguageModel, backoff:LanguageModel, alpha:Double) extends LanguageModel {
    def order = main.order
    def vocab = main.vocab
    def probability(word:String, history:String*) = {
        alpha * main.probability(word,history:_*) +
          (1 - alpha) * backoff.probability(word,"")
    }

  }

  case class ImprovedPreDistanceBarAwareLM(vocab: Set[String],a: Double,countMap:Map[Int, Double],countTotalMap:HashMap[Int, Double]) extends LanguageModel {
    def order = 20

    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*):Double = {
      var dist = order - 1
            if(history(history.length - 1)=="[/BAR]"){
              if(word == "[BAR]") {
                1
              } else {
                0
              }
            } else {
              for (i <- 0 until history.length - 1) {
                if (history(i) == "[BAR]") {
                  dist = order - 2 - i
                }
              }
              val p = (countMap(dist) + a) / (countTotalMap(dist) + a * order)
              //      val p = (countMap(dist)+a)/5611.0
              //      println(dist+","+countMap(dist)+","+countTotalMap(dist)+" " + p)
              if (word == "[/BAR]") {
                p
              } else if (word == "[BAR]") {
                0
              }
              else {
                (1 - p) / (vocab.size - 2)
              }
            }
    }
  }

  case class PreDistanceBarAwareLM(vocab: Set[String],a: Double,countMap:Map[Int, Double],countTotalMap:HashMap[Int, Double]) extends LanguageModel {
    def order = 20

    //TODO: This needs to be improved by you.
    def probability(word: String, history: String*):Double = {
      var dist = order - 1

        for (i <- 0 until history.length) {
          if (history(i) == "[BAR]") {
            dist = order - 2 - i
          }
        }
        val p = (countMap(dist) + a) / (countTotalMap(dist) + a * order)
        if (word == "[/BAR]") {
          p
        }
        else {
          (1 - p) / (vocab.size - 1)
        }

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
//    val devOOV = Util.replaceOOVs(Util.OOV, vocab, dev)

    //TODO: Improve the MyBarAwareLM implementation
    val bestParamForMyBarAwareLM = 0.014084507042253521
    val myBarLM = MyBarAwareLM(vocab,bestParamForMyBarAwareLM)
    val bestPPForMyBarAwareLM = LanguageModel.perplexity(myBarLM, dev)
//    for(i <- 21 to 150 by 2){
//      val a = 1.0/i
//      val myBarLM = MyBarAwareLM(vocab,a)
//      val pp = LanguageModel.perplexity(myBarLM, dev)
//      println(a + "," + pp)
//      if(pp<bestPPForMyBarAwareLM){
//        bestParamForMyBarAwareLM = a
//        bestPPForMyBarAwareLM = pp
//      }
//    }
    println("Best linear parameter and PP for My Bar Aware LM are :" + bestParamForMyBarAwareLM + " , " + bestPPForMyBarAwareLM)

  val countMap:Map[Int, Double] = {
    val content = Source.fromFile(trainFile).getLines().mkString(" ")
    val bars = content.split("\\[/BAR\\] \\[BAR\\]")
    val distanceArray = new  ArrayBuffer[Int]()
    bars.foreach(bar=>{
      if (bar==" ") {
        distanceArray += 0
      } else {
        val l = bar.trim.split(" ").length
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

    val bestParamForPreDistLM = 23.0
    val preDistLM = PreDistanceBarAwareLM(vocab,bestParamForPreDistLM,countMap,countTotalMap)
    val bestPPForPreDistLM = LanguageModel.perplexity(preDistLM, dev)
//    var bestPPForPreDistLM = Double.MaxValue
//    for(i <- 1.0 to 40.0 by 1){
//      val a = i
//      val preDistLM = PreDistanceBarAwareLM(vocab,a,countMap,countTotalMap)
//      val pp = LanguageModel.perplexity(preDistLM, dev)
//      if(pp<bestPPForPreDistLM){
//        bestParamForPreDistLM = a
//        bestPPForPreDistLM = pp
//      }
//    }
    println("Best smoothing parameter and PP for Pre-Distance LM are :" + bestParamForPreDistLM + " , " + bestPPForPreDistLM)

    val bestParamForImprovedPreDistLM = 16.0
    val improvedPreDistLM = ImprovedPreDistanceBarAwareLM(vocab,bestParamForImprovedPreDistLM,countMap,countTotalMap)
    val bestPPForImprovedPreDistLM = LanguageModel.perplexity(improvedPreDistLM, dev)
//    var bestPPForImprovedPreDistLM = Double.MaxValue
//
//    for(i <- 1.0 to 40.0 by 1){
//      val a = i
//      val improvedPreDistLM = ImprovedPreDistanceBarAwareLM(vocab,a,countMap,countTotalMap)
//      val pp = LanguageModel.perplexity(improvedPreDistLM, dev)
//      if(pp<bestPPForImprovedPreDistLM){
//        bestParamForImprovedPreDistLM = a
//        bestPPForImprovedPreDistLM = pp
//      }
//    }
    println("Best smoothing parameter and PP for Improved Pre-Distance LM are :" + bestParamForImprovedPreDistLM + " , " + bestPPForImprovedPreDistLM)

    val bestParamForInterpolatedLM = 0.16666666666666666
    val uniGramLM = NGramLM(trainOOV, 1)
    val interpolatedLM = InterpolatedLM(improvedPreDistLM,uniGramLM, bestParamForInterpolatedLM)
    val bestPPForInterpolatedLM = LanguageModel.perplexity(interpolatedLM, dev)
//    var bestPPForInterpolatedLM = Double.MaxValue
//    for(i <- 1 to 50 ) {
//      val a = 1.0/i
//      val improvedPreDistLM = ImprovedPreDistanceBarAwareLM(vocab, bestParamForImprovedPreDistLM, countMap, countTotalMap)
//      val uniGramLM = NGramLM(trainOOV, 1)
//      val interpolatedLM = InterpolatedLM(improvedPreDistLM,uniGramLM,a)
//      val pp = LanguageModel.perplexity(interpolatedLM, dev)
//      if(pp<bestPPForInterpolatedLM){
//        bestParamForInterpolatedLM = a
//        bestPPForInterpolatedLM = pp
//      }
//    }
    println("Best interpolated parameter alpha and PP for Interpolated LM are :" + bestParamForInterpolatedLM + " , " + bestPPForInterpolatedLM)
//    val improvedPreDistLM = ImprovedPreDistanceBarAwareLM(vocab, bestParamForImprovedPreDistLM, countMap, countTotalMap)
//    val uniGramLM = NGramLM(trainOOV, 1)
//    val interpolatedLM = InterpolatedLM(improvedPreDistLM,uniGramLM,bestParamForInterpolatedLM)
    println(LanguageModel.sample(interpolatedLM, List("[BAR]"), 1000).mkString(" "))

    //      println(vocab.toSeq.map(w=>{
//        if(w=="[/BAR]")
//        println(w,barLm.probability(w,"[BAR]"))
//        barLm.probability(w,"[BAR]")
//      }).sum)


//      println(LanguageModel.perplexity(barLm, devOOV))
//      println(LanguageModel.perplexity(nGramLm, devOOV))
//      for (i <- (1 until 100)) {
//        val alpha = 1.0/i
//        println(LanguageModel.perplexity(BarInterpolatedLM(barLm,nGramLm,0.5), devOOV))
//      }

//    println(LanguageModel.sample(BarInterpolatedLM(barLm,nGramLm,0.5), List("[BAR]"), 10000).mkString(" "))


    //      val pp = LanguageModel.perplexity(lm, dev)
//      println(a+","+pp)
//    }


  //    println(lm.probability("[/BAR]","[BAR] [ Verse 1 ] { J-Live } [/BAR] [BAR] [OOV] I see y 'all through these speakers and"))

    //This calculates the perplexity of the




    //TODO:

    //TODO: combine a unigram model with the BAR aware LM through interpolation.
    //TODO: Improve the BarAwareLM to give probability 1 to a [BAR] following a [/BAR], and 0 otherwise.

  }
}
