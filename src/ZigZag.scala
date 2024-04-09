
import ZigZagUtils.{Board, Direction, completeBoardRandomly, getUserInput, initializeGameBoardWithWordsFromFile, play, printGameOver, printGameState, printGameStateList, printNewGame, printRules, randomChar, showPrompt, stringToDirection}

import scala.annotation.tailrec
import scala.util.Random

case class GameState(numTries: Int, numFound: Int, board: Board)

object ZigZag extends App {

  private val initialBoard: Board = List.fill(5)(List.fill(5)('.'))

  private val initialRandom: MyRandom = MyRandom(10)

  private var (filledBoard, updatedRandom) = completeBoardRandomly(initialBoard, initialRandom, ZigZagUtils.randomChar)
  filledBoard = initializeGameBoardWithWordsFromFile(filledBoard)
  private val initialState = GameState(0, 0, filledBoard)

  printRules()
  mainLoop(initialState, updatedRandom, List())

  @tailrec
  def mainLoop(gameState: GameState, random: MyRandom, hist: List[GameState]): Unit = {
    printGameState(gameState)
    showPrompt()
    val userInput = getUserInput.toUpperCase

    userInput match {
      case "Q" =>
        printGameOver()
        printGameState(gameState)
        println("History: ")
        printGameStateList(hist)

      case "N" =>
        printGameOver()
        val (newBoard, newRandom) = completeBoardRandomly(initialBoard, random, randomChar)
        mainLoop(GameState(0, 0, newBoard), newRandom, hist :+ gameState)

      case "R" =>
        printRules()
        mainLoop(gameState, random, hist)

      case _ =>
        var found: Int = gameState.numFound
        var tries : Int = gameState.numTries
        try {
          print("x: ")
          val x = getUserInput.toInt
          print("y: ")
          val y = getUserInput.toInt
          print("\ndirection: ")
          val directionStr = getUserInput.toUpperCase
          stringToDirection(directionStr) match {
            case Some(direction) =>
              tries = tries + 1
              if (play(gameState.board, userInput, (y, x), direction)) {
                found = found + 1
                println("Correct!")
              } else {
                println("Try again!")
              }
            case None => println("Invalid direction")
          }
        } catch {
          case _: NumberFormatException => println("Invalid coordinates")
        }
        val newGameState = gameState.copy(tries, found)
        mainLoop(newGameState, random, hist)
    }
  }

}
