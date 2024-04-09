import ZigZagUtils.{Board, Coord2D, Direction, completeBoardRandomly, fillOneCell, initializeGameBoardWithWordsFromFile, play, randomChar, readFromFile, setBoardWithWords}

import java.io.File

object Main {

  val r = MyRandom(114320)
  def main(args: Array[String]): Unit = {
    // Example of usage:
    val board: Board = List(
      List('.', '.', '.'),
      List('.', '.', '.'),
      List('.', '.', '.')
    )



    val updatedBoard = initializeGameBoardWithWordsFromFile(board)

    val (b, r1) = completeBoardRandomly(updatedBoard, r, _.nextChar)

    b.foreach(row => println(row.mkString(" ")))
  }




}