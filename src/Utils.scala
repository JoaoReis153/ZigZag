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


}
