import scala.io.Source

object Usage {

  private def txtFileToSeq(filePath: String): Seq[String] = {
    val fileSource = Source.fromFile(filePath)
    val words = fileSource.getLines
      .flatMap(_.split("\\W+"))
      .map(_.toLowerCase)
      .filter(_.nonEmpty) // Filter out empty strings
      .toSeq
    fileSource.close()
    words
  }

  private def estimate_AMS_Simplified(words: Seq[String], k: Int, r: Int): Int = {
    val ams = new AMS_Simplified(words.length, r)
    words.foreach(ams.add)
    val kthMomentEstimate = ams.estimateMoment(k)
    kthMomentEstimate.toInt
  }

  private def ExactMoment(k: Int, words: Seq[String]) = {
    val em = new WordMoments()
    val kth_moment = em.countMoment(k, words)
    kth_moment
  }
  def main(args: Array[String]): Unit = {
    val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/canterbury/alice29.txt"
    val words = txtFileToSeq(filePath)
    val M0 = ExactMoment(0, words)
    val EsM0 = estimate_AMS_Simplified(words, 0, 60)
    val M1 = ExactMoment(1, words)
    val EsM1 = estimate_AMS_Simplified(words, 1, 60)
    val M2 = ExactMoment(2, words)
    val EsM2 = estimate_AMS_Simplified(words, 2, 60)
    val M3 = ExactMoment(3, words)
    val EsM3 = estimate_AMS_Simplified(words, 3, 60)

//    println("Exact 1st: " + M1)
//    println("Estimate_Simply 1st: " + EsM1)
//    println("Rate: " + EsM1.toDouble / M1.toDouble)
//    println(" ")
    println("Exact 2nd: " + M2)
    println("Estimate_Simply 2st: " + EsM2)
    println("Rate: " + EsM2.toDouble / M2.toDouble)
    println(" ")
    println("Exact 3nd: " + M3)
    println("Estimate_Simply 3st: " + EsM3)
    println("Rate: " + EsM3.toDouble/M3.toDouble)
    println(" ")
  }
}
