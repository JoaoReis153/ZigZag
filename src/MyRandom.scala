import scala.util.Random

case class MyRandom(seed: Long) extends Random {

  private val letterA = 65;
  private val alphabetSize = 26;
  def nextChar: (Char, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val char = (newSeed % alphabetSize + letterA).toChar // Generates a random uppercase letter
    (char, nextRandom)
  }

}
