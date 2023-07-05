import scala.io.Source
import scala.util.Random.nextInt

object Usage {

  private def txtFileToSeq(filePath: String): Seq[String] = {
    val fileSource = Source.fromFile(filePath)
    val words = fileSource.mkString.toLowerCase().split("\\W+").filter(_.nonEmpty).toSeq
    fileSource.close()
    words
  }

  private def estimate_AMS_Simplified(words: Seq[String], k: Int): BigInt = {
    val amsS = new AMS_Simplified(words.length)
    words.foreach(amsS.add)
    val kthMomentEstimate = amsS.estimateMoment(k)
    kthMomentEstimate
  }

  private def estimate_AMS(words: Seq[String], k: Int, epsilon: Double, lambda: Double): BigInt = {
    val ams = new AMS_New_Alg(k, epsilon, lambda)
    words.foreach(ams.add)
    val kthMomentEstimate = ams.estimateMoment(k)
    kthMomentEstimate
  }

  private def ExactMoment(k: Int, words: Seq[String]) = {
    val em = new WordMoments()
    val kth_moment = em.countMoment(k, words)
    kth_moment
  }
  def main(args: Array[String]): Unit = {
    val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/large/bible.txt"
    val words = txtFileToSeq(filePath)

    val epsilon = 0.98 // probability
    val lambda = 0.01 // relative error

    val M1 = ExactMoment(1, words)
    val EsM1 = estimate_AMS_Simplified(words, 1)
    val EM1 = estimate_AMS(words, 1, epsilon, lambda)

    val M2 = ExactMoment(2, words)
    val EsM2 = estimate_AMS_Simplified(words, 2)
    val EM2 = estimate_AMS(words, 2, epsilon, lambda)

    val M3 = ExactMoment(3, words)
    val EsM3 = estimate_AMS_Simplified(words, 3)
    val EM3 = estimate_AMS(words, 3, epsilon, lambda)

    // Printing header

    printf("| %2s | %15s | %20s | %15s | %15s | %15s |\n", "k", "Exact Moment", "Simplify AMS Estimate", "Error", "AMS Estimate", "Error")
    println("|----|-----------------|-----------------------|-----------------|-----------------|-----------------|")

    // Printing rows

    printf("| %2d | %15d | %21d | %15.4f | %15s | %15.4s |\n", 1, M1, EsM1, (math.abs(EsM1.toDouble - M1.toDouble) / M1.toDouble), EM1, (math.abs(EM1.toDouble - M1.toDouble) / M1.toDouble))
    printf("| %2d | %15d | %21d | %15.4f | %15d | %15.4f |\n", 2, M2, EsM2, (math.abs(EsM2.toDouble - M2.toDouble) / M2.toDouble), EM2, (math.abs(EM2.toDouble - M2.toDouble) / M2.toDouble))
    printf("| %2d | %15d | %21d | %15.4f | %15d | %15.4f |\n", 3, M3, EsM3, (math.abs(EsM3.toDouble - M3.toDouble) / M3.toDouble), EM3, (math.abs(EM3.toDouble - M3.toDouble) / M3.toDouble))
  }
}
