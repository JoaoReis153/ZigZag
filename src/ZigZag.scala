
import ZigZagUtils.{Board, Direction, GameState, completeBoardRandomly, getWordList, getUserInput, initializeGameBoardWithWordsFromFile, play, printGameOver, printGameState, printGameStateList, printNewGame, printRules, randomChar, showPrompt}

import scala.annotation.tailrec


object ZigZag extends App {

  private val initialBoard: Board = List.fill(5)(List.fill(5)('.'))

  private val currentTime = System.currentTimeMillis()

  private val initialRandom: MyRandom = MyRandom(currentTime)

  private var (filledBoard, updatedRandom) = completeBoardRandomly(initialBoard, initialRandom, ZigZagUtils.randomChar)
  filledBoard = initializeGameBoardWithWordsFromFile(filledBoard)
  
  private val initialState = GameState(0, 0, filledBoard)

  printRules()
  mainLoop(initialState, updatedRandom)

  @tailrec
  private def mainLoop(gameState: GameState, random: MyRandom): Unit = {
    printGameState(gameState)
    showPrompt()
    val userInput = getUserInput.toUpperCase

    userInput match {
      case "Q" =>
        printGameOver()
        printGameState(gameState)

      case "N" =>
        printGameOver()
        var (newBoard, newRandom) = completeBoardRandomly(initialBoard, MyRandom(currentTime), randomChar)
        newBoard = initializeGameBoardWithWordsFromFile(newBoard)
        printNewGame()
        mainLoop(GameState(0, 0, newBoard), newRandom)


      case "R" =>
        printRules()
        mainLoop(gameState, random)

      case _ =>
        val totalWords = getWordList().length

        val nextState: Option[GameState] = try {
          print("x: ")
          val x = getUserInput.toInt
          print("y: ")
          val y = getUserInput.toInt
          print("\nDirection: (north, northeast, east, southeast, south, southwest, west, northwest)\n")
          val directionStr = getUserInput.toUpperCase
          Direction.stringToDirection(directionStr) match {
            case Some(direction) =>
              val newTries = gameState.numTries + 1
              if (play(gameState.board, userInput, (y, x), direction)) {
                if (gameState.foundWords.contains(userInput)) {
                  println(s"'$userInput' was already found!")
                  Some(gameState.copy(numTries = newTries))
                } else {
                  val newFound      = gameState.numFound + 1
                  val newFoundWords = gameState.foundWords + userInput
                  val newState      = GameState(newTries, newFound, gameState.board, newFoundWords)
                  println("----> Correct! <----")
                  if (newFound == totalWords) {
                    printGameState(newState)
                    println(s"\n=== You found all $totalWords words in $newTries tries! ===")
                    printGameOver()
                    None
                  } else {
                    println(s"${totalWords - newFound} word(s) remaining.")
                    Some(newState)
                  }
                }
              } else {
                println("Try again!")
                Some(gameState.copy(numTries = newTries))
              }
            case None =>
              println("Invalid direction")
              Some(gameState)
          }
        } catch {
          case _: NumberFormatException =>
            println("Invalid coordinates")
            Some(gameState)
        }
        nextState match {
          case Some(state) => mainLoop(state, random)
          case None        => ()
        }
    }
  }

}
