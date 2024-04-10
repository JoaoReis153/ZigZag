import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.matching.Regex

object ZigZagUtils {

  type Board = List[List[Char]]
  private type Coord2D = (Int, Int) //(linha, coluna)

  object Direction extends Enumeration {
    type Direction = Value
    private val North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest = Value

    def nextCoord(coord: Coord2D, direction: Direction.Value): Coord2D = direction match {
      case Direction.North => (coord._1 - 1, coord._2)
      case Direction.NorthEast => (coord._1 - 1, coord._2 + 1)
      case Direction.East => (coord._1, coord._2 + 1)
      case Direction.SouthEast => (coord._1 + 1, coord._2 + 1)
      case Direction.South => (coord._1 + 1, coord._2)
      case Direction.SouthWest => (coord._1 + 1, coord._2 - 1)
      case Direction.West => (coord._1, coord._2 - 1)
      case Direction.NorthWest => (coord._1 - 1, coord._2 - 1)
    }


    def stringToDirection(input: String): Option[Direction.Value] = {
      Direction.values.find(_.toString.equalsIgnoreCase(input))
    }
  }






  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    rand.nextChar
  }

  private def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {

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


  private def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

    @tailrec
    def fillWord(board: Board, word: String, positions: List[Coord2D], index: Int): Board = positions match {
      case Nil => board
      case head :: tail => fillWord(fillOneCell(board, word(index), head), word, tail, index + 1)
    }

    @tailrec
    def fillAllWords(board: Board, words: List[String], positions: List[List[Coord2D]], index: Int): Board = words match {
      case Nil => board
      case head :: tail => fillAllWords(fillWord(board, head, positions(index), 0), tail, positions, index + 1)
    }

    fillAllWords(board, words, positions, 0)
  }


  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = {

    def completeBoardFillRow(row: List[Char], r: MyRandom): (List[Char], MyRandom) = {
      row.foldLeft((List[Char](), r)) {
        //case (accumulator, currentElement)
        case ((accRow, accR), _) =>
          val (newChar, newR) = f(accR)
          (accRow :+ newChar, newR)
      }
    }

    def completeBoardAux(board: Board, r: MyRandom): (Board, MyRandom) = board match {
      case Nil => (Nil, MyRandom(System.currentTimeMillis()))
      case head :: tail =>
        val (newHead, r1) = completeBoardFillRow(head, r)
        val (newTail, r2) = completeBoardAux(tail, r1)
        (newHead :: newTail, r2)
    }

    completeBoardAux(board, r)
  }


  def play(board: Board, word: String, start: Coord2D, direction: Direction.Value): Boolean = {
    val file: String = "src/givenWords.txt"
    val (words, positions) = readFromFile(file)
    @tailrec
    def searchForTheGivenWordInTheList(wordsList: List[String], coordsList: List[List[Coord2D]], coordAnswer: Coord2D, wordAnswer: String, direction: Direction.Value): Boolean = wordsList match {
      case Nil =>
        false
      case x :: xs =>
        if (x == wordAnswer && coordsList.head.head == coordAnswer && Direction.nextCoord(coordAnswer, direction) == coordsList.head.tail.head) {
          checkWordIsInBoard(board, wordAnswer, coordsList.head)
        } else {
          searchForTheGivenWordInTheList(xs, coordsList.tail, coordAnswer, wordAnswer, direction)
        }
    }

    @tailrec
    def checkWordIsInBoard(board: Board, wordAnswer: String, coordsList: List[Coord2D]): Boolean = coordsList match {
      case Nil => true
      case x :: xs =>
        if (wordAnswer.head != board(x._1)(x._2)) {
          false
        } else {
          checkWordIsInBoard(board, wordAnswer.tail, xs)
        }
    }

    searchForTheGivenWordInTheList(words, positions, start, word, direction)
  }


  private def readFromFile(file: String): (List[String], List[List[Coord2D]]) = {
    val bufferedSource = Source.fromFile(file)
    val content: List[String] = bufferedSource.getLines.mkString("\n").split("\n").toList
    bufferedSource.close()

    val coordPattern: Regex = "\\(([0-9]+),([0-9]+)\\)".r
    val wordPattern: Regex = "^[a-zA-Z]+$".r

    @tailrec
    def getFormatFromList( content: List[String], words: List[String], coordinates: List[List[Coord2D]], currentCoords: List[Coord2D]): (List[String], List[List[Coord2D]]) = content match {
      case Nil =>
        // If there are leftover coordinates, add them to the list; otherwise, just return what we have
        if (currentCoords.nonEmpty) (words, coordinates :+ currentCoords) else (words, coordinates)
      case x :: xs => x match {
        case "" =>
          // Empty string, indicating end of current word's coordinates
          getFormatFromList(xs, words, coordinates :+ currentCoords, List.empty)
        case coordPattern(y, x) =>
          // Coordinate line
          val newCoord = (x.toInt, y.toInt)
          getFormatFromList(xs, words, coordinates, currentCoords :+ newCoord)
        case _ if wordPattern.matches(x) =>
          var word = x.toUpperCase
          // Word line
          if (currentCoords.isEmpty) {
            // No current coordinates, just add the word
            getFormatFromList(xs, words :+ word, coordinates, currentCoords)
          } else {
            // There are some coordinates, finalize them and start fresh for the new word
            getFormatFromList(xs, words :+ word, coordinates :+ currentCoords, List.empty)
          }
        case _ =>
          // Ignore any line that doesn't match expected patterns (could log or handle errors here)
          getFormatFromList(xs, words, coordinates, currentCoords)
      }
    }
    // Start the recursive processing
    getFormatFromList(content, List(), List(), List())

  }


  def initializeGameBoardWithWordsFromFile(board: Board): Board = {
    val file: String = "src/givenWords.txt"
    val (words, positions) = readFromFile(file)

    setBoardWithWords(board, words, positions)
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


  def printGameStateList(lst: List[GameState]): String = {
    lst match {
      case Nil => ("")
      case head :: tail => printGameState(head) + printGameStateList(tail)
    }
  }


  def getUserInput: String = readLine.trim.toUpperCase


  def printGameOver(): Unit = println("\n=== GAME OVER ===")


  def printNewGame(): Unit = ("\n=== NEW GAME ===")


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
