package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.Tokenizer
import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.Util

/**
 * This stub is provided as help for you to solve the first question.
 * @author riedel, yingwen
 */
object TokenizationQuestion {

  def main(args: Array[String]) {

    //the directory that contains the raw documents
    val dataDir = new File("data/")

    //the file that contains the target gold tokenization
    val goldFile = new File("data/assignment1/p1/goldtokens.txt")

    //the actual raw documents
    val raw = Assignment1Util.loadTokenizationQuestionDocs(dataDir)


    val quoteSuffix = "'000|'7(?!6)|'76|'80s|'84|'91|'A(?!(LL|ll|ngel|-yo))|'A-yo|'ALL|'All|'Angel(?!-ist)|'Angel-ist|'Bout|'Fo(?=')|'C(?!(an|ause|uz))|'Can|'Cause|'Cuz|'F(?!(o|ore))|'Fo(?!re)|'Fore|'Hey|'I|'MON~|'MON(?!~)|'N(?!uff)|'Nuff|'Oh|'Pac|'Riq|'S(?!is)|'Sis|'The|'Til(?!l)|'Till|(?<!tryin)'a(?!ll)|'all|'bendago|'bout|'cause|'cept|'cha|'chu|'cross|'yeah|'d|'em|'er|'fore|'fro|'fuckas|'fuckers|'gain|'gnac|'gonna|'gotta|'graphs|'gwan|'head|'hens|'him|'hin|'ho(?!od)|'hood|'iminals|'in(?!g)|'ing|'know(?!msayin)|'knowmsayin|'lax|'lem|(?<!(a-drenalin|man))'ll|'ly|'m(?!(a|atic|ember|ent|on|ma|sayin))|'ma(?!tic)|'matic|'member|'ment|'mma|'mon|'msayin|'n(?!(a|ahI|eath|ess|et|other|uff))|'nuff|'na(?!hI)|'nahI|'neath|'ness|'net|'nother|'ol|'pon|'posed|'re(?!al)|'real|'ribbit|'riq|'round|'ry(?!body)|'rybody|(?<!n)'s(?!(e|pose|posed|tead|tead|-his-name))|'s-his-name|'se|'spose(?!d)|'sposed|'stead|'tender|'til(?!l)|'till|'tin|'ve|'ybody|'yeah|'yo|'-on|'-five|'/sev"
    val quotePrefix = "(?<!n)'(?=( |\\[|,|!))"
    val punctuation = quotePrefix+ "|"+ quoteSuffix + "|n't(?!(a|cha))|n'tcha|n'ta|\\[.?BAR\\]|\\[(?!.?BAR)|(?<!.?BAR)\\]|\\+|\\(|\\)|;|:|\\?|\\}|\\{|!|\\*|,|_|(?<!(Dr|Mr))\\.|\""
    val afterSign = s"(?<=$punctuation)"
    val beforeSign = s"(?<!\\s)(?=$punctuation)"

    //TODO: The tokenizer you need to improve to match the gold tokenization
    val tokenizer = Tokenizer.fromRegEx(s"(\\s|$beforeSign|$afterSign)")

    //the result of the tokenization
    val result = Util.words(raw map tokenizer)

    //the gold tokenization file
    val gold = Assignment1Util.loadWords(goldFile).toBuffer

//    gold.foreach(x => println(x))


    //we find the first pair of tokens that don't match
    val mismatch = result.zip(gold).find { case (a, b) => a != b }

    //Your goal is to make sure that mismatch is None
    mismatch match {
      case None => println("Success!")
      case Some(pair) => println("The following tokens still don't match: " + pair)
    }

  }

}
