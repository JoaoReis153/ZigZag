import scala.annotation.tailrec
import scala.io.StdIn.readLine

object ZigZagUtils {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest = Value
  }

  private def nextCoord(coord: Coord2D, direction: Direction.Value): Coord2D = direction match {
    case Direction.North => (coord._1 - 1, coord._2)
    case Direction.South => (coord._1 + 1, coord._2)
    case Direction.East => (coord._1, coord._2 + 1)
    case Direction.West => (coord._1, coord._2 - 1)
    case Direction.NorthEast => (coord._1 - 1, coord._2 + 1)
    case Direction.NorthWest => (coord._1 - 1, coord._2 - 1)
    case Direction.SouthEast => (coord._1 + 1, coord._2 + 1)
    case Direction.SouthWest => (coord._1 + 1, coord._2 - 1)
  }

  def stringToDirection(input: String): Option[Direction.Value] = {
    Direction.values.find(_.toString.equalsIgnoreCase(input))
  }
  def randomChar(rand: MyRandom) : (Char, MyRandom) = {
    rand.nextChar
  }

  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {

    def fillRow(row: List[Char], columnIndex: Int): List[Char] = row match {
      case Nil => Nil
      case _ :: tail if columnIndex == 0 => letter :: tail
      case head :: tail => head :: fillRow(tail, columnIndex - 1)
    }

    def fillBoard(board: Board, rowIndex: Int): Board = board match {
      case Nil => Nil
      case head :: tail if rowIndex == 0 => fillRow(head, coord._2) :: tail
      case head :: tail => head :: fillBoard(tail, rowIndex - 1)
    }

    fillBoard(board, coord._1)
  }

  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

    @tailrec
    def fillWord(board: Board, word : String, positions: List[Coord2D], index: Int): Board = positions match {
      case Nil => board
      case head :: tail => fillWord(fillOneCell(board, word(index), head) , word, tail, index + 1)
    }

    @tailrec
    def fillAllWords(board: Board, words: List[String], positions: List[List[Coord2D]], index: Int): Board = words match {
      case Nil => board
      case head :: tail => fillAllWords( fillWord(board, head, positions(index), 0) , tail, positions, index + 1)
    }

    fillAllWords(board, words, positions, 0)
  }


  def completeBoardRandomly(board:Board, r:MyRandom, f: MyRandom => (Char, MyRandom)):(Board, MyRandom) = {

      def completeBoardFillRow(row: List[Char], r: MyRandom): (List[Char], MyRandom) = {
        row.foldLeft((List[Char](), r)) {
          //case (accumulator, currentElement)
          case ((accRow, accR), x) =>
            if (x == '.') {
              val (newChar, newR) = f(accR)
              (accRow :+ newChar, newR)
            } else {
              (accRow :+ x, accR)
            }
        }
      }

      def completeBoardAux(board: Board, r: MyRandom): (Board, MyRandom) = board match {
        case Nil => (Nil, r)
        case head :: tail =>
          val (newHead, r1) = completeBoardFillRow(head, r)
          val (newTail, r2) = completeBoardAux(tail, r1)
          (newHead :: newTail, r2)
      }
      completeBoardAux(board, r)
  }



  def play(board: Board, word: String, start: Coord2D, direction: Direction.Value): Boolean = {

    @tailrec
    def checkWord(coord: Coord2D, word: String): Boolean = {
      if (word.isEmpty) true
      else if (coord._1 < 0 || coord._1 >= board.length || coord._2 < 0 || coord._2 >= board(coord._1).length) false
      else if (board(coord._1)(coord._2) != word.head) false
      else checkWord(nextCoord(coord, direction), word.tail)
    }

    checkWord(start, word)

  }

  def showPrompt(): Unit = {
    print("\nGuess a word: ")
  }


  def printGameState(gameState: GameState): Unit = {
    println(s"\nNumber of Tries: ${gameState.numTries}")
    println(s"Number of found words: ${gameState.numFound}")

    @tailrec
    def printBoard(board: Board): Unit = board match {
      case Nil => // Base case: no more rows to print
      case head :: tail => // Recursive case: print the current row and proceed to the next
        println(head.mkString(" "))
        printBoard(tail) // Recursively print the rest of the board
    }

    printBoard(gameState.board)
  }

  def printGameStateList(lst : List[GameState]): String = {
    lst match {
      case Nil => ("")
      case head::tail => printGameState(head) + printGameStateList(tail)
    }
  }
  def getUserInput: String = readLine.trim.toUpperCase

  def printGameOver(): Unit = println("\n=== GAME OVER ===")

  def printNewGame(): Unit = println("\n=== NEW GAME ===")

  def printRules(): Unit = {
    println("\n-----------------------------------------")
    println("Welcome to ZigZag!")
    println("The rules are simple, find the hidden words.")
    println("(to play a new game press N)")
    println("(to quit press Q)")
    println("(to check the match history press H)")
    println("(to check the rules once again press R :)")
    println("Good luck, have fun!")
    println("-----------------------------------------")
  }
}
