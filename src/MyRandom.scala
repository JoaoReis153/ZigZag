import scala.util.Random

case class MyRandom(seed: Long) extends Random {

  private val letterA = 65;
  private val letterZ = 90;
  def nextChar: (Char, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val randomInt = (newSeed >>> 16).toInt
    val randomIntInRange = letterA + randomInt%(letterZ - letterA + 1)
    (randomIntInRange.toChar, nextRandom)
  }

}
