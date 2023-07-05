import scala.util.Random

class AMS_New_Alg(k: Int, epsilon: Double, lambda: Double) {
  private val r = (1 / (epsilon * epsilon)).toInt
  private val counts = scala.collection.mutable.Map[String, Int]()
  private var totalElements = 0

  def add(word: String): Unit = {
    totalElements += 1
    if (Random.nextDouble() < r.toDouble / totalElements) {
      counts(word) = 1
    } else if (counts.contains(word)) {
      counts(word) = counts(word) + 1
    }
  }

  def estimateMoment(k: Int): BigInt = {
    println("r = " + r)
    val momentEstimate = counts.values.map(count => BigInt(math.pow(count, k).toInt) - BigInt(math.pow(count - 1, k).toInt))
    val mk = BigInt(totalElements) / BigInt(counts.size) * momentEstimate.sum
    mk
  }

}
