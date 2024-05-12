import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.matching.Regex
import java.io._

object ZigZagUtils {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(linha, coluna)

  def checkCoord(c: (Int,Int), board: Board): Boolean = {
    if (c._2 >= 0 && c._2 < board.length && c._1 >= 0 && c._1 < board.length)
      return true
    false
  }


  // Direções possíveis
  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest = Value

    // Retorna a coordenada seguinte dada uma coordenada e direção
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

    // Converte uma string em uma direção
    // Usar Option da nos um modo de lidar com a ausência de elementos
    def stringToDirection(input: String): Direction.Value = {
      input.toLowerCase() match {
        case "north" => North
        case "northeast" => NorthEast
        case "east" => East
        case "southeast" => SouthEast
        case "south" => South
        case "southwest" => SouthWest
        case "west" => West
        case _ => NorthWest
      }
    }

    // Calcula a direção entre duas coordenadas e lança um erro se estiverem mais distantes do que um quadrado
    def calculateDirection(startCoord: Coord2D, endCoord: Coord2D): Direction.Value = {
      val deltaX = endCoord._2 - startCoord._2
      val deltaY = endCoord._1 - startCoord._1

      if (math.abs(deltaX) > 1 || math.abs(deltaY) > 1)
        throw new IllegalArgumentException("Coordinates are more than one square away.")

      if (deltaX == 0 && deltaY == -1) North
      else if (deltaX == 1 && deltaY == -1) NorthEast
      else if (deltaX == 1 && deltaY == 0) East
      else if (deltaX == 1 && deltaY == 1) SouthEast
      else if (deltaX == 0 && deltaY == 1) South
      else if (deltaX == -1 && deltaY == 1) SouthWest
      else if (deltaX == -1 && deltaY == 0) West
      else if (deltaX == -1 && deltaY == -1) NorthWest
      else throw new IllegalArgumentException("Invalid coordinates.")
    }
  }

  private def isValidCoord(board: Board, coord: Coord2D): Boolean =
    coord._1 >= 0 && coord._1 < board.length && coord._2 >= 0 && coord._2 < board(coord._1).length


  // Gera um caractere aleatório
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    rand.nextChar
  }

  // Preenche uma posição do tabuleiro com um caractere específico
  private def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {

    // Preenche uma linha do tabuleiro
    def fillRow(row: List[Char], columnIndex: Int): List[Char] = row match {
      case Nil => Nil
      case _ :: tail if columnIndex == 0 => letter :: tail
      case head :: tail => head :: fillRow(tail, columnIndex - 1)
    }

    // Preenche o tabuleiro
    def fillBoard(board: Board, rowIndex: Int): Board = board match {
      case Nil => Nil
      case head :: tail if rowIndex == 0 => fillRow(head, coord._2) :: tail
      case head :: tail => head :: fillBoard(tail, rowIndex - 1)
    }

    fillBoard(board, coord._1)
  }

  private def getOneCell(board: Board, coord: Coord2D): Char = {
    val (x, y) = coord
    if (isValidCoord(board, coord)) {
      board(x)(y)
    } else {
      '.' // Returning '.' for out of bounds
    }
  }

  // Preenche o tabuleiro com as palavras nas posições dadas
  private def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {

    // Preenche o tabuleiro nas posições dadas com a letra correspondente
    @tailrec
    def fillWord(board: Board, word: String, positions: List[Coord2D], index: Int): Board = positions match {
      case Nil => board
      case head :: tail => fillWord(fillOneCell(board, word(index), head), word, tail, index + 1)
    }

    // Preenche o tabuleiro com as palavras dadas
    @tailrec
    def fillAllWords(board: Board, words: List[String], positions: List[List[Coord2D]], index: Int): Board = words match {
      case Nil => board
      case head :: tail => fillAllWords(fillWord(board, head, positions(index), 0), tail, positions, index + 1)
    }

    fillAllWords(board, words, positions, 0)
  }

// Preenche o tabuleiro com caracteres aleatórios
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = {

    // Preenche uma linha do tabuleiro com caracteres aleatorios
    def completeBoardFillRow(row: List[Char], r: MyRandom): (List[Char], MyRandom) = {
      row.foldLeft((List[Char](), r)) {
        //case (acumulador, elementoAtual)
        case ((accRow, accR), _) =>
          val (newChar, newR) = f(accR)
          (accRow :+ newChar, newR)
      }
    }

    //Prenche o tabuleiro
    def completeBoard(board: Board, r: MyRandom): (Board, MyRandom) = board match {
      case Nil => (Nil, MyRandom(System.currentTimeMillis()))
      case head :: tail =>
        val (newHead, r1) = completeBoardFillRow(head, r)
        val (newTail, r2) = completeBoard(tail, r1)
        (newHead :: newTail, r2)
    }

    completeBoard(board, r)
  }

  private def searchCloseCoordinates(board: Board, word: String, start: Coord2D, visitedCoordinates : List[Coord2D]): Boolean = {
    if (word.isEmpty) {
      return true
    }
    if (visitedCoordinates.contains(start) || getOneCell(board, start) != word.head) {
      false
    } else {
      val newVisited = start :: visitedCoordinates
      val directions = List(Direction.North, Direction.NorthEast, Direction.East, Direction.SouthEast,
        Direction.South, Direction.SouthWest, Direction.West, Direction.NorthWest)

      directions.exists { dir =>
        val nextCoord = Direction.nextCoord(start, dir)
        searchCloseCoordinates(board, word.tail, nextCoord, newVisited)
      }
    }
  }

  private def getWordPositions(word: String): List[Coord2D] = {
    val (words, positions) = readWordsAndCoordinatesFromFile()

    @tailrec
    def getWordPositionsAux(wordsList: List[String], coordsList: List[List[Coord2D]], word: String): List[Coord2D] = (wordsList, coordsList) match {
      case (Nil, Nil) => List()
      case (x :: xs, y :: ys) =>
        if (x == word) y
        else getWordPositionsAux(xs, ys, word)
    }
    getWordPositionsAux(words, positions, word)
  }

  def play(board: Board, word: String, start: Coord2D, direction: Direction.Value): (Boolean, List[Coord2D]) = {
    val newCoord = Direction.nextCoord(start, direction)
    println("checkCoordd "+ checkCoord(newCoord, board))
    println("Geton cell "  + getOneCell(board, start) == word.head)
    println("Get second cell " + getOneCell(board, newCoord) == word.tail.head)
    println("Check word in board " +checkWordInBoard(board, word, start))
    println("Search close coordinates " + searchCloseCoordinates(board, word.tail, newCoord, List()))
    if(checkCoord(newCoord, board) && getOneCell(board, start) == word.head && getOneCell(board, newCoord) == word.tail.head && checkWordInBoard(board, word, start) && searchCloseCoordinates(board, word.tail, newCoord, List())) {
        (true, getWordPositions(word))
    }
    else (false, Nil)

  }


  def playGUI(board: Board, word: String, start: Coord2D, secondCoordinate: Coord2D): (Boolean, List[Coord2D]) = {
    play(board, word, start, Direction.calculateDirection(start, secondCoordinate))
  }

// Joga a palavra, na posição inicial, segundo a direção dada
  private def checkWordInBoard(board: Board, word: String, start: Coord2D): Boolean = {

    // Extrai todas as palavras e respetivas posições do ficheiro
    val (words, positions) = readWordsAndCoordinatesFromFile()

    // Procura a palavra jogada nas palavras extraidas do ficheiro
    // Quando encontrar chama a função "checkWordIsInBoard"
    @tailrec
    def searchForTheGivenWordInTheList(board: Board, wordsList: List[String], coordsList: List[List[Coord2D]], coordAnswer: Coord2D, wordAnswer: String): Boolean = wordsList match {
      case Nil => false
      case x :: xs =>
        if (x == wordAnswer) {
          if(!checkLettersInBoard(board, x, coordsList.head)) throw new IllegalArgumentException("As letras da palavra " + x + " não estão nas posições correspondentes.")
          else true
        } else {
          searchForTheGivenWordInTheList(board, xs, coordsList.tail, coordAnswer, wordAnswer)
        }
    }

    // Verifica se para cada posição da palavra está a letra suposta
    @tailrec
    def checkLettersInBoard(board: Board, wordAnswer: String, coordsList: List[Coord2D]): Boolean = coordsList match {
      case Nil => true
      case x :: xs =>
        if (wordAnswer.head != getOneCell(board, (x._1,x._2))) {
          throw new IllegalArgumentException("A letra _" + wordAnswer.head+ "_ da palavra " + word + " foi sobreposta na posição (" + x + " ")
        } else {
          checkLettersInBoard(board, wordAnswer.tail, xs)
        }
    }

    searchForTheGivenWordInTheList(board, words, positions, start, word)

  }


  private def checkBoard(board: Board):Boolean = {
    printBoard(board, List())
    val (words, positions) = readWordsAndCoordinatesFromFile()

    @tailrec
    def checkWords(board: Board, wordsList: List[String], coordsList: List[List[Coord2D]]): Boolean = (wordsList, coordsList) match {
      case (Nil, Nil) => true
      case (word :: xs, coord :: ys) =>
        if (checkWordInBoard(board, word, coord.head)) checkWords(board, xs, ys)
        else false
    }

    def playInEveryDirection(board: Board, word: String, start: Coord2D): Int = {
      val directions = List(Direction.North, Direction.NorthEast, Direction.East, Direction.SouthEast,
        Direction.South, Direction.SouthWest, Direction.West, Direction.NorthWest)
      directions.map { dir =>
        if (play(board, word, start, dir)._1) 1 else 0
      }.sum
    }


    @tailrec
    def processRow(board: Board, word: String, rowIndex: Int, columnIndex: Int = 0, acc: Int = 0): Int = {
      if (columnIndex >= board(rowIndex).length) acc
      else {
        val start = (rowIndex, columnIndex)
        val count = playInEveryDirection(board, word, start)
        processRow(board, word, rowIndex, columnIndex + 1, acc + count)
      }
    }

    @tailrec
    def processBoard(board: Board, word: String, rowIndex: Int = 0, acc: Int = 0): Int = {
      if (rowIndex >= board.length) acc
      else {
        val count = processRow(board, word, rowIndex)
        processBoard(board, word, rowIndex + 1, acc + count)
      }
    }

    @tailrec
    def checkNoDuplicates(board: Board, wordsList: List[String]): Boolean = wordsList match {
      case Nil => true
      case x :: xs =>
        val counter = processBoard(board, x)
        if (counter > 1) {
          throw new IllegalArgumentException("Palavra " + x + " duplicada " + counter + " vezes")
        } else {
          checkNoDuplicates(board, xs)
        }
    }

    checkWords(board, words, positions) && checkNoDuplicates(board, words)
  }


  def readRandomFromFile(): MyRandom = {
    val file = "src/seed.txt"
    val bufferedSource = Source.fromFile(file)
    val line = bufferedSource.getLines().next()
    bufferedSource.close()
    MyRandom(line.toLong)
  }

  def writeRandomInFile(myRandom: MyRandom): Unit = {
    val file = "src/seed.txt"
    val pw = new PrintWriter(new File(file))
    pw.write(myRandom.toString)
    pw.close()
  }


  // Extrai as palavras e respetivas coordenadas do ficheiro
  private def readWordsAndCoordinatesFromFile(): (List[String], List[List[Coord2D]]) = {
    val bufferedSource = Source.fromFile("src/givenWords.txt")

    //Extrai o conteudo
    val content: List[String] = bufferedSource.getLines.mkString("\n").split("\n").toList
    bufferedSource.close()

    // Padrões regex
    val coordPattern: Regex = "\\(([0-9]+),([0-9]+)\\)".r
    val wordPattern: Regex = "^[a-zA-Z]+$".r

    @tailrec
    def getFormatFromList( content: List[String], words: List[String], coordinates: List[List[Coord2D]], currentCoords: List[Coord2D]): (List[String], List[List[Coord2D]]) = content match {
      case Nil =>
        // Se ainda houverem coordenadas, adiciona-as à lista
        // Caso contrário devolve o que já temos
        if (currentCoords.nonEmpty) (words, coordinates :+ currentCoords) else (words, coordinates)
      case x :: xs => x match {
        case "" =>
          // String vazia, indica o fim das coordenadas da palavra atual
          getFormatFromList(xs, words, coordinates :+ currentCoords, List.empty)
        case coordPattern(y, x) =>
          // Caso encontremos o padrão das coordenadas
          // Adicionamo-la às coordenadas da palavra atual
          val newCoord = (x.toInt, y.toInt)
          getFormatFromList(xs, words, coordinates, currentCoords :+ newCoord)
        case _ if wordPattern.matches(x) =>
          // Caso encontremos o padrão das palavras
          // Adicionamo-la à lista das palavras
          val word = x.toUpperCase
          if(word.length < 2) throw new IllegalArgumentException("Found a word too small")
          if(word.length > 25) throw new IllegalArgumentException("Found a word too big")
          getFormatFromList(xs, words :+ word, coordinates, currentCoords)
        case _ =>
          //Qualquer linha fora do esperado, é ignorada
          getFormatFromList(xs, words, coordinates, currentCoords)
      }
    }
    getFormatFromList(content, List(), List(), List())
  }


  // Inicializa o tabuleiro com as palavras do ficheiro
  def initializeGameBoardWithWordsFromFile(board: Board): Board = {
    val (words, positions) = readWordsAndCoordinatesFromFile()
    val newBoard = setBoardWithWords(board, words, positions)
    if(!checkBoard(newBoard))
      throw new IllegalArgumentException("Board wrongly formated, it may just be bad luck.")

    newBoard
  }


  // O resto das funções apenas mostram frases do ecrã e são auto-explicativas



  def showPrompt(): Unit = {
    print("\nGuess a word: ")
  }


  def printGameState(gameState: GameState): Unit = {
    println(s"\nTries: ${gameState.numTries}")
    println(s"Found: ${gameState.numFound}\n")

    // Helper function to determine if a coordinate should be colored green
    printBoard(gameState.board, gameState.greenCoordinates)
  }

  def isGreenCoord(x: Int, y: Int, greenCoords: List[Coord2D]): Boolean = {
    greenCoords.contains((x, y))
  }

  @tailrec
  def printBoard(board: Board, greenCoords: List[Coord2D], currentRow: Int = 0): Unit = board match {
    case Nil =>
    case head :: tail =>
      head.zipWithIndex.foreach {
        case (char, colIndex) =>
          if (isGreenCoord(currentRow, colIndex, greenCoords)) {
            print(s"\u001B[32m$char\u001B[0m ")
          } else {
            print(s"$char ")
          }
      }
      println()
      printBoard(tail, greenCoords, currentRow + 1)
  }


  private def printGameStateList(lst: List[GameState]): String = {
    lst match {
      case Nil => ""
      case head :: tail => printGameState(head) + printGameStateList(tail)
    }
  }


  def getUserInput: String = readLine.trim.toUpperCase


  def printGameOver(): Unit = println("\n=== GAME OVER ===")

  def printNewGame(): Unit = "\n=== NEW GAME ==="


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
