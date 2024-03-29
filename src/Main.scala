import Utils.{Board, Coord2D, completeBoardRandomly, fillOneCell, randomChar, setBoardWithWords}

object Main {

  val r = MyRandom(11420)
  def main(args: Array[String]): Unit = {
    // Example of usage:
    val board: Board = List(
      List('.', '.', '.'),
      List('.', '.', '.'),
      List('.', '.', '.')
    )

    val updatedBoard = setBoardWithWords(board, List("CAT"), List(List((0, 0), (1, 1), (0, 2))))

    val (b, r1) = completeBoardRandomly(updatedBoard, r, _.nextChar)

    b.foreach(row => println(row.mkString(" ")))
  }




}