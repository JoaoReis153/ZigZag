import scala.util.Random

case class MyRandom(seed: Long) extends Random {

  private val letterA = 65;
  private val letterZ = 90;
  def nextChar: (Char, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val char = (newSeed % 26 + 65).toChar // Generates a random uppercase letter
    (char, nextRandom)
  }

}
