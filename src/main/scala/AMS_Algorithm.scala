import scala.util.Random

class AMS_Algorithm(k: Int, epsilon: Double, lambda: Double, m: Int = 27333, n: Int = 2579) {
  private val a = k * math.log(1.0/epsilon)
  private val b = lambda * lambda
  private val c = math.pow(n, 1.0-1.0/k)
  private val r: Int = ((a / b) * c).toInt
  private val randomPositions: Set[Int] = (1 to r).map(_ => Random.nextInt(m) + 1).toSet
  private val positionIndices = randomPositions.zipWithIndex.toMap
  private val counts: Array[Int] = new Array[Int](r)
  private val words: Array[String] = new Array[String](r)
  private var position: Int = 0

  def add(word: String): Unit = {
    position += 1
    positionIndices.get(position) match {
      case Some(index) =>
        words(index) = word
        counts(index) += 1
      case None =>
        val wordIndex = words.indexOf(word)
        if (wordIndex >= 0) {
          counts(wordIndex) += 1
        }
    }
  }

  def estimateMoment(k: Int): BigInt = {
    println("r = " + r)
    val momentEstimate = counts.map(x => (BigInt(math.pow(x, k).toInt) - BigInt(math.pow(x - 1, k).toInt)))
    val mk = BigInt(position) / BigInt(r) * momentEstimate.sum
    mk
  }
}
