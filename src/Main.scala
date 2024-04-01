import Utils.{Board, Coord2D, Direction, completeBoardRandomly, fillOneCell, play, randomChar, setBoardWithWords}

object Main {

  val r = MyRandom(114320)
  def main(args: Array[String]): Unit = {
    // Example of usage:
    val board: Board = List(
      List('.', '.', '.'),
      List('.', '.', '.'),
      List('.', '.', '.')
    )

    val updatedBoard = setBoardWithWords(board, List("CAT"), List(List((0, 0), (0, 1), (0, 2))))

    val (b, r1) = completeBoardRandomly(updatedBoard, r, _.nextChar)

    println(play(b, "CAT", (0,0), Direction.East))

    b.foreach(row => println(row.mkString(" ")))
  }




}