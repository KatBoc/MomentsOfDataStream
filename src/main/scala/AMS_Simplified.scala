import scala.util.Random

/**
 * The AMS_Simplified class is a simplified implementation of the Alon-Matias-Szegedy (AMS) Algorithm
 * for estimating moments of a distribution in a data stream.
 *
 * @param streamLength The length of the data stream.
 * @param numSamples The number of random indices to sample from the data stream. Defaults to 60.
 */
class AMS_Simplified(streamLength: Int, numSamples: Int = 60) {
  private val randomPositions: Set[Int] = (1 to numSamples).map(_ => Random.nextInt(streamLength) + 1).toSet
  private val counts: Array[Int] = new Array[Int](numSamples)
  private val words: Array[String] = new Array[String](numSamples)
  private var position : Int = 0

  /**
   * Adds an element to the data stream and updates the internal counters.
   *
   * @param word The element to add to the data stream.
   */
  def add(word: String): Unit = {
    position += 1
    if (randomPositions.contains(position)) {
      val positionIndex = randomPositions.toList.indexOf(position)
      words(positionIndex) = word
      counts(positionIndex) += 1
    } else {
      val wordIndex = words.indexOf(word)
      if (wordIndex >= 0) {
        counts(wordIndex) += 1
      }
    }
  }

  /**
   * Estimates the k-th frequency moment of the data stream.
   *
   * @param k The order of the frequency moment to be estimated.
   * @return The estimated k-th frequency moment of the data stream.
   */
  def estimateMoment(k: Int): BigInt = {
    val momentEstimate = counts.map(x => (BigInt(math.pow(x, k).toInt) - BigInt(math.pow(x - 1, k).toInt)))
    val mk = BigInt(streamLength) / BigInt(numSamples) * momentEstimate.sum
    mk
  }
}
