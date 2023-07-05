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

  private def estimate_AMS(words: Seq[String], k: Int, epsilon: Double, delta: Double): BigInt = {
    val ams = new AMS_Algorithm(k, epsilon, delta)
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
    val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/canterbury/alice29.txt"
    val words = txtFileToSeq(filePath)

    // estimates to be within 1% of the true value with 95% confidence.
    val epsilon = 0.1
    val delta = 0.1

/*    val M0 = ExactMoment(0, words)
    val EsM0 = estimate_AMS_Simplified(words, 0)
    val M1 = ExactMoment(1, words)
    val EsM1 = estimate_AMS_Simplified(words, 1)*/

    val M2 = ExactMoment(2, words)
    val EsM2 = estimate_AMS_Simplified(words, 2)
    val EM2 = estimate_AMS(words, 2, epsilon, delta)
    val M3 = ExactMoment(3, words)
    val EsM3 = estimate_AMS_Simplified(words, 3)
    val EM3 = estimate_AMS(words, 3, epsilon, delta)


    // Printing header

    printf("| %2s | %15s | %20s | %15s | %15s | %15s |\n", "k", "Exact Moment", "Simplify AMS Estimate", "Error Rate", "AMS Estimate", "Rate AMS/E")
    println("|----|-----------------|-----------------------|-----------------|-----------------|-----------------|")

    // Printing rows
/*
    printf("| %2d | %15d | %21d | %15s | %15s | %15s |\n",     0, M0, EsM0, "N/A", "N/A", "N/A")
    printf("| %2d | %15d | %21d | %15.4f | %15s | %15s |\n",   1, M1, EsM1, EsM1.toDouble / M1, "N/A", "N/A")*/
    printf("| %2d | %15d | %21d | %15.4f | %15d | %15.2f |\n", 2, M2, EsM2, (math.abs(EsM2.toDouble - M2.toDouble) / M2.toDouble), EM2, (math.abs(EM2.toDouble - M2.toDouble) / M2.toDouble))
    printf("| %2d | %15d | %21d | %15.4f | %15d | %15.2f |\n", 3, M3, EsM3, (math.abs(EsM3.toDouble - M3.toDouble) / M3.toDouble), EM3, (math.abs(EM3.toDouble - M3.toDouble) / M3.toDouble))
  }
}
