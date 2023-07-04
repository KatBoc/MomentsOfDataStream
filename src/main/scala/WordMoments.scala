import scala.io.Source
import math.pow
class WordMoments() {
  def countMoment(k: Int, words: Seq[String]) = {
    val wordCounts = words.groupBy(identity).mapValues(_.length)
    val topWords = wordCounts.toSeq.sortBy(-_._2)
    val kth_moment = topWords.map(i => math.pow(i._2, k)).sum.toInt
    kth_moment
  }
}