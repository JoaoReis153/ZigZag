import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.matching.Regex

object ZigZagUtils {

  type Board = List[List[Char]]
  private type Coord2D = (Int, Int) //(row, column)

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest = Value

    def nextCoord(coord: Coord2D, direction: Direction.Value): Coord2D = direction match {
      case Direction.North => (coord._1 - 1, coord._2)
      case Direction.South => (coord._1 + 1, coord._2)
      case Direction.East => (coord._1, coord._2 + 1)
      case Direction.West => (coord._1, coord._2 - 1)
      case Direction.NorthEast => (coord._1 - 1, coord._2 + 1)
      case Direction.NorthWest => (coord._1 - 1, coord._2 - 1)
      case Direction.SouthEast => (coord._1 + 1, coord._2 + 1)
      case Direction.SouthWest => (coord._1 + 1, coord._2 - 1)
    }
  }


  def stringToDirection(input: String): Option[Direction.Value] = {
    Direction.values.find(_.toString.equalsIgnoreCase(input))
  }
  def randomChar(rand: MyRandom) : (Char, MyRandom) = {
    rand.nextChar
  }

  def completeBoardRandomly(board:Board, r:MyRandom, f: MyRandom => (Char, MyRandom)):(Board, MyRandom) = {

    def completeBoardFillRow(row: List[Char], r: MyRandom): (List[Char], MyRandom) = {
      row.foldLeft((List[Char](), r)) {
        //case (accumulator, currentElement)
        case ((accRow, accR), _) =>
          val (newChar, newR) = f(accR)
          (accRow :+ newChar, newR)
      }
    }

    def completeBoardAux(board: Board, r: MyRandom): (Board, MyRandom) = board match {
      case Nil => (Nil,  MyRandom(System.currentTimeMillis()))
      case head :: tail =>
        val (newHead, r1) = completeBoardFillRow(head, r)
        val (newTail, r2) = completeBoardAux(tail, r1)
        (newHead :: newTail, r2)
    }

    completeBoardAux(board, r)
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

  private def readFromFile(file: String): (List[String], List[List[(Int, Int)]]) = {
    val bufferedSource = Source.fromFile(file)
    try {
      val content = bufferedSource.getLines().mkString("\n")
      val blocks = content.split("\n\n").toList // Split by empty line to get blocks of word and coords

      val wordPattern: Regex = "^[a-zA-Z]+$".r
      val coordPattern: Regex = "\\(([0-9]+),([0-9]+)\\)".r

      // Process each block to extract the word and its coordinates
      val processedBlocks = blocks.map { block =>
        val lines = block.split("\n").toList

        val word = lines.headOption.getOrElse("") // First line of each block should be the word
        val coords = lines.tail.flatMap {
          case coordPattern(col, row) => Some((row.toInt, col.toInt))
          case _ => None
        }

        (word, coords)
      }

      // Separate the words and coordinates into their respective lists
      processedBlocks.foldLeft((List.empty[String], List.empty[List[(Int, Int)]])) {
        case ((wordsAcc, coordsAcc), (word, coords)) =>
          (wordsAcc :+ word.toUpperCase, coordsAcc :+ coords)
      }
    } finally {
      bufferedSource.close()
    }
  }

  def initializeGameBoardWithWordsFromFile(board: Board): Board = {
    val file: String = "src/givenWords.txt"
    val (words, positions) = readFromFile(file)
    setBoardWithWords(board, words, positions)
  }

  private def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

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


  def play(board: Board, word: String, start: Coord2D, direction: Direction.Value): Boolean = {
    val file: String = "src/givenWords.txt"
    val (words, positions) = readFromFile(file)

    @tailrec
    def searchForTheGivenWordInTheList(wordsList: List[String], coordsList: List[List[Coord2D]], coordAnswer: Coord2D, wordAnswer: String, direction: Direction.Value): Boolean = wordsList match {

      case Nil =>
        false
      case x::xs =>
        if(x == wordAnswer && coordsList.head.head == coordAnswer && Direction.nextCoord(coordAnswer, direction) == coordsList.head.tail.head) {
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
