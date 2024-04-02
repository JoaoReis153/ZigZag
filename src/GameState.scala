
import ZigZagUtils.{getUserInput, printGameOver, printNewGame, showPrompt}

import scala.annotation.tailrec
import scala.util.Random

case class GameState(numFlips: Int, numCorrect: Int)

object ZigZag extends App {

  val r = MyRandom(10)
  val s = GameState(0, 0)

  mainLoop(s, r, List())

  @tailrec
  def mainLoop(gameState: GameState, random: MyRandom, hist: List[GameState]) {
    //def mainLoop(gameState: GameState, random: Random) {

    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {


      case "Q" | "q"  => {
        printGameOver()
        printGameState(gameState)
        println("History: ")
        printGameStateList(hist)
        // return out of the recursion here
      }

      case "N" | "n" | "R" | "r" => {
        printGameOver()
        printGameState(gameState)
        printNewGame()
        val newHist = hist :+ gameState
        mainLoop(GameState(0,0), random, newHist)

      }

      case _ => {
        print("Invalid key")
        mainLoop(gameState, random, hist)
      }
    }
  }

}
