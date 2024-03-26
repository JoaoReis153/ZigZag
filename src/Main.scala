import Utils.randomChar

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")

    val initialRandom = MyRandom(100L) // Seed with 100 for example
    val (char1, nextRandom1) = randomChar(initialRandom) // Generates the first random character and the next random state
    val (char2, nextRandom2) = randomChar(nextRandom1) // Generates another random character using the new state
    val (char3, nextRandom3) = randomChar(nextRandom2) // Generates another random character using
    println(char1)
    println(char2)
    println(char3)



  }




}