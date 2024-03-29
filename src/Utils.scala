import scala.annotation.tailrec

object Utils {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)
  /*
  type Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West, NorthEast, NortWest, SouthEast, SouthWest = Value
  }
  */

  def randomChar(rand: MyRandom) : (Char, MyRandom) = {
    rand.nextChar
  }

  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {
    def fillRow(row: List[Char], columnIndex: Int): List[Char] = row match {
      case Nil => Nil
      case head :: tail if columnIndex == 0 => letter :: tail
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
      def completeBoardFillRow(row: List[Char], r: MyRandom): (List[Char], MyRandom) = row match {
        case Nil => (Nil, r)
        case head :: tail if head == '.' =>
          val (newRow, newR) = completeBoardFillRow(tail, r)
          val (char, r1) = f(newR)
          (char :: newRow, r1)
        case head :: tail =>
          val (newRow, newR) = completeBoardFillRow(tail, r)
          (head :: newRow, newR)
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

}
