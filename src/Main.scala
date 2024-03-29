import Utils.{Board, fillOneCell, randomChar, setBoardWithWords, Coord2D}

object Main {
  
  def main(args: Array[String]): Unit = {
    // Example of usage:
    val board1: Board = List(
      List('A', 'B', 'C'),
      List('D', 'E', 'F'),
      List('G', 'H', 'I')
    )

    val updatedBoard1 = setBoardWithWords(board1, List("CAT"), List(List((0, 0), (1, 1), (0, 2))))

    updatedBoard1.foreach(row => println(row.mkString(" ")))

  }




}