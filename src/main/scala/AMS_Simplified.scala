import scala.util.Random

class AMS_Simplified(streamLength: Int, numSamples: Int = 60) {
  private val randomPositions: Set[Int] = (1 to numSamples).map(_ => Random.nextInt(streamLength) + 1).toSet
  private val counts: Array[Int] = new Array[Int](numSamples)
  private val words: Array[String] = new Array[String](numSamples)
  private var position : Int = 0

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

  def estimateMoment(k: Int): BigInt = {
    val momentEstimate = counts.map(x => (BigInt(math.pow(x, k).toInt) - BigInt(math.pow(x - 1, k).toInt)))
    val mk = BigInt(streamLength) / BigInt(numSamples) * momentEstimate.sum
    mk
  }
}
