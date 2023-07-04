import scala.util.Random

/**
 * AMS_Simplified implements the Alon-Matias-Szegedy sketch algorithm
 * to estimate the moments of the distribution of elements in a stream of data.
 *
 * @param streamLength Length of the data stream.
 * @param numSamples Number of random indices to sample.
 */
class AMS_Simplified(streamLength: Int, numSamples: Int) {
  private val randomPositions: Array[Int] = Array.fill(numSamples)(Random.nextInt(streamLength))
  private val counts: Array[Int] = new Array[Int](numSamples)
  private val words: Array[String] = new Array[String](numSamples)
  private var position = 0

  /**
   * Adds a word to the sketch.
   *
   * @param word The word to add.
   */
  def add(word: String): Unit = {
    position += 1
    if (randomPositions.contains(position))
      {
        val i = randomPositions.indexOf(position)
        words(i) = word
        counts(i) += 1
      } else if (words.contains(word)) {
      counts(words.indexOf(word)) += 1
    }
  }

  /**
   * Estimates the k-th moment of the data stream's distribution.
   *
   * @param momentOrder The order of the moment to estimate.
   * @return The estimated value of the k-th moment.
   */
  def estimateMoment(momentOrder: Int): BigInt = {
    val momentEstimate = counts.map(x => BigInt(math.pow(x, momentOrder).toInt) - BigInt(math.pow(x - 1, momentOrder).toInt)).sum
    BigInt(streamLength / numSamples) * momentEstimate
  }
}