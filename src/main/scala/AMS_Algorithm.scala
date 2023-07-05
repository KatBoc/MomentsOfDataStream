import scala.util.Random

class AMS_Algorithm(k: Int, epsilon: Double, delta: Double, m: Int = 27333, n: Int = 2579) {
  private val r: Int = ((k * math.log(1 / epsilon) / (delta * delta)) * math.pow(n, 1 - (1 / k))).toInt
  private val randomPositions: Set[Int] = (1 to r).map(_ => Random.nextInt(m) + 1).toSet
  private val counts: Array[Int] = new Array[Int](r)
  private val words: Array[String] = new Array[String](r)
  private var position: Int = 0

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

//  def add(word: String): Unit = {
//    position += 1
//
//    // Find index of existing word
//    val indexWithWord = R.indexWhere(_._2 == word)
//
//    // If word exists, increment the count
//    if (indexWithWord >= 0) {
//      val (_, _, count) = R(indexWithWord)
//      R(indexWithWord) = (position, word, count + 1)
//    } else {
//      // Find an empty position
//      val indexEmpty = R.indexWhere(_._1 == 0)
//
//      // If an empty position exists and sampling condition is met, insert the word
//      if (indexEmpty >= 0 && Random.nextDouble() < (r.toDouble / position)) {
//        R(indexEmpty) = (position, word, 1)
//      }
//    }
//
//    // If position is greater than m, with probability (r/m+1) remove one old entry in R
//    if (position > m && Random.nextDouble() < (r.toDouble / (m + 1))) {
//      val indexToRemove = Random.nextInt(R.length)
//      R(indexToRemove) = (0, "", 0) // Resetting the entry
//    }
//  }

  def estimateMoment(k: Int): BigInt = {
    val momentEstimate = counts.map(x => (BigInt(math.pow(x, k).toInt) - BigInt(math.pow(x - 1, k).toInt)))
    val mk = BigInt(m) / BigInt(m) * momentEstimate.sum
    mk
  }
}