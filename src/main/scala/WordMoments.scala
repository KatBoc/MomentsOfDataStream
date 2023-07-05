import scala.io.Source
import math.pow

/**
 * The WordMoments class provides functionality for computing the k-th frequency moment
 * of a sequence of words.
 */
class WordMoments() {
  /**
   * Computes the exact k-th frequency moment of a sequence of words.
   * In this algorithm, whole data are stored.
   *
   * @param k The order of the frequency moment to be computed.
   * @param words A sequence of words for which the k-th frequency moment is to be computed.
   * @return The k-th frequency moment of the given sequence of words.
   */
  def countMoment(k: Int, words: Seq[String]) = {
    val wordCounts = words.groupBy(identity).mapValues(_.length)
    val topWords = wordCounts.toSeq.sortBy(-_._2)
    val kth_moment = topWords.map(i => math.pow(i._2, k)).sum.toInt
    kth_moment
  }
}