package uk.ac.ucl.cs.mr.statnlpbook.assignment1

import java.io.File

import uk.ac.ucl.cs.mr.statnlpbook.corpora._

import scala.io.Source

/**
 * @author riedel
 * @author mbosnjak
 */
object Assignment1Util {

  // your dataDir is probably "data/" if you downloaded the data there
  def loadTokenizationQuestionDocs(dataDir: File) = {
    val artistDir = new File(dataDir, "/ohhla/www.ohhla.com/anonymous/")

    val jlive = OHHLA.allAlbumDirs(new File(artistDir, OHHLA.JLive.id)).sortBy(_.getName) flatMap OHHLA.loadDirRaw
    val roots = OHHLA.allAlbumDirs(new File(artistDir, OHHLA.Roots.id)).sortBy(_.getName) flatMap OHHLA.loadDirRaw
    jlive ++ roots
  }

  def loadWords(txtFile:File) = {
    val source = Source.fromFile(txtFile)
    source.getLines().flatMap(_.split(" ")).toList
  }


  case class Instance(lyrics: String, author: Option[String] = None, prediction: Option[String] = None)


  def loadDataset(file: File) = {
    val lines = Source.fromFile(file).getLines()
    lines.map{ x =>
      val split = x.split("\t")
      if (split.length == 1)
        Instance(split(0))
      else
        Instance(split(1), Some(split(0)))
    }.toSeq
  }

}
