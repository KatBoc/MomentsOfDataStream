import scala.io.Source

def txtFileToSeq(filePath: String): Seq[String] = {
  val fileSource = Source.fromFile(filePath)
  val words = fileSource.getLines
    .flatMap(_.split("\\W+"))
    .map(_.toLowerCase)
    .filter(_.nonEmpty) // Filter out empty strings
    .toSeq
  fileSource.close()
  words
}


val filePath = "C:/Users/KatarzynaBocian(2399/Desktop/BDA/Stream programming/A/Stream_programming_assigments/src/canterbury-corpus-master/canterbury/alice29.txt"
val words = txtFileToSeq(filePath)
words.length

