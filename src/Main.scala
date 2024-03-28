import Utils.{Board, fillOneCell, randomChar}

object Main {
  def main(args: Array[String]): Unit = {
    // Example of usage:
    val board: Board = List(
      List('A', 'B', 'C'),
      List('D', 'E', 'F'),
      List('G', 'H', 'I')
    )
    val newBoard = fillOneCell(board, 'X', (2, 2)) // Changing the element at position (1,1) to 'X'

    newBoard.foreach(row => println(row.mkString(" ")))
  }




}