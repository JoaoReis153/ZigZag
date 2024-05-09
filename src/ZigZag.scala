import ZigZagUtils._


import scala.annotation.tailrec

case class GameState(numTries: Int, numFound: Int, board: Board, greenCoordinates: List[Coord2D])

object ZigZag extends App {

  val initialBoard: Board = List.fill(5)(List.fill(5)('.'))

  val initialRandom: MyRandom = ZigZagUtils.readRandomFromFile();

  private val  (filledRandomBoard, updatedRandom) = completeBoardRandomly(initialBoard, initialRandom, ZigZagUtils.randomChar)
  private val filledBoard = initializeGameBoardWithWordsFromFile(filledRandomBoard)

  private val initialState = GameState(0, 0, filledBoard, List())

  printRules()
  mainLoop(initialState, updatedRandom)

  private def mainLoop(gameState: GameState, random: MyRandom): Unit = {
    printGameState(gameState)
    showPrompt()
    val userInput = getUserInput.toUpperCase

    userInput match {
      case "Q" =>
        printGameOver()
        printGameState(gameState)

      case "N" =>
        printNewGame()
        writeRandomInFile(random)
        val (newRandomBoard, newRandom) = completeBoardRandomly(initialBoard, random, randomChar)
        val newBoard = initializeGameBoardWithWordsFromFile(newRandomBoard)
        printNewGame()
        mainLoop(GameState(0, 0, newBoard, List()), newRandom)


      case "R" =>
        printRules()
        mainLoop(gameState, random)

      case _ =>
        try {
          print("x: ")
          val x = getUserInput.toInt
          print("y: ")
          val y = getUserInput.toInt
          val coord = (y,x)

          print("\nDirection: (north,  northeast, east, southeast, south, southwest, west, northwest\n")
          val directionStr = getUserInput.toUpperCase
          val direction = Direction.stringToDirection(directionStr)

          if (checkCoord(coord, initialBoard) && play(gameState.board, userInput, coord, direction)._1) {
            println("Green coordinates: " + play(gameState.board, userInput, coord, direction)._2)
            val newGameState = gameState.copy(gameState.numTries + 1, gameState.numFound + 1, gameState.board,  gameState.greenCoordinates ++ play(gameState.board, userInput, coord, direction)._2)
            println()
            println("----> Correct! <----")
            mainLoop(newGameState, random)
          } else {
            val newGameState = gameState.copy(gameState.numTries + 1, gameState.numFound)
            println("Try again!")
            mainLoop(newGameState, random)
          }
        } catch {
          case _: NumberFormatException => println("Invalid coordinates")
        }
        val newGameState = gameState.copy(gameState.numTries, gameState.numFound)
        mainLoop(newGameState, random)
    }
  }

}
