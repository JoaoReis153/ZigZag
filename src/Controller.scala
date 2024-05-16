import ZigZag.{initialBoard, initialRandom}
import ZigZagUtils.{Board, Coord2D, completeBoardRandomly, initializeGameBoardWithWordsFromFile, playGUI, randomChar}
import javafx.scene.paint.Color
import javafx.scene.control.{Button, Label, TextField}
import javafx.fxml.FXML
import javafx.scene.layout.GridPane
import javafx.event.ActionEvent
import javafx.application
import javafx.application.Platform

class Controller {
  @FXML private var gridBoard: GridPane = _
  private val initialBoard: Board = List.fill(5)(List.fill(5)('.')) // Tabuleiro inicial vazio
  private val initialRandom: MyRandom = ZigZagUtils.readRandomFromFile() // Carregando a semente aleatória inicial
  private var board: Board = _


  @FXML private var lblTries: Label = _
  @FXML private var lblFound: Label = _
  @FXML private var lblPontos: Label = _

  @FXML var txtWord: TextField = _

  @FXML private var btn00: Button = _
  @FXML private var btn01: Button = _
  @FXML private var btn02: Button = _
  @FXML private var btn03: Button = _
  @FXML private var btn04: Button = _
  @FXML private var btn10: Button = _
  @FXML private var btn11: Button = _
  @FXML private var btn12: Button = _
  @FXML private var btn13: Button = _
  @FXML private var btn14: Button = _
  @FXML private var btn20: Button = _
  @FXML private var btn21: Button = _
  @FXML private var btn22: Button = _
  @FXML private var btn23: Button = _
  @FXML private var btn24: Button = _
  @FXML private var btn30: Button = _
  @FXML private var btn31: Button = _
  @FXML private var btn32: Button = _
  @FXML private var btn33: Button = _
  @FXML private var btn34: Button = _
  @FXML private var btn40: Button = _
  @FXML private var btn41: Button = _
  @FXML private var btn42: Button = _
  @FXML private var btn43: Button = _
  @FXML private var btn44: Button = _

  private var selectedButtons: List[Button] = Nil
  private var selectedButtonsCoord: List[Coord2D] = Nil

  private val selectedButtonColor = Color.DARKORANGE

  private def disableButtonsForWord(wordCoordinates: List[Coord2D]): Unit = {
    wordCoordinates.foreach { coord =>
      val (row, col) = coord
      val button = getButton(row, col)
      button.setDisable(true)
    }
  }


  def handleButtonClick(event: ActionEvent): Unit = {
    val clickedButton: Button = event.getSource.asInstanceOf[Button]
    val row: Int = GridPane.getRowIndex(clickedButton)
    val col: Int = GridPane.getColumnIndex(clickedButton)

    val buttonCoord : Coord2D = (col, row)

    val button = getButton(row, col)
    if (button.isDisable || selectedButtons.length >= 2) return

    button.setStyle("-fx-background-color: " + colorToHex(selectedButtonColor))
    selectedButtons = clickedButton :: selectedButtons
    selectedButtonsCoord = buttonCoord :: selectedButtonsCoord
    button.setDisable(true)

    if (selectedButtons.length == 2) {
      disableAllButtons(selectedButtons)
    }
  }

  def handlePlayButtonClick(): Unit = {
    // Coordenadas dos botões selecionados
    val start = selectedButtonsCoord.tail.head
    val secondCoordinate = selectedButtonsCoord.head

    // Chama playGUI com as coordenadas e verifica se a palavra foi encontrada
    val (wordFound, _) = playGUI(board, txtWord.getText, start, secondCoordinate)
    val (_,wordCoordinates) = playGUI(board, txtWord.getText, start, secondCoordinate)
    // Reativa os botões e limpa a lista de botões selecionados
    enableAllButtons(selectedButtons)
    selectedButtons = Nil
    selectedButtonsCoord = Nil

    // Incrementa o número de tentativas
    val currentTries = lblTries.getText.toInt
    lblTries.setText((currentTries + 1).toString)

    // Se uma palavra for encontrada, incrementa o número de palavras encontradas

    if (wordFound) {
      val currentFound = lblFound.getText.toInt
      lblFound.setText((currentFound + 1).toString)

      val score = calculateScore(currentFound + 1,currentTries + 1)
      lblPontos.setText(score.toString)

     // disableButtonsForWord(wordCoordinates) // Desativa os botões que formam a palavra
    }
  }

  def calculateScore(found: Int, tries: Int): Int = {
    if (found == 0)
      throw new IllegalArgumentException("Cannot divide by zero.")
    else
      (found * 50) / tries
  }

  def handleButtonNewGameClick(): Unit = {
    // Incrementa a semente do arquivo de texto
    val updatedRandom = MyRandom(System.currentTimeMillis())
    ZigZagUtils.writeRandomInFile(updatedRandom)

    // Reinicia o tabuleiro com letras aleatórias usando a nova semente
    val (randomBoard, _) = ZigZagUtils.completeBoardRandomly(initialBoard, updatedRandom, ZigZagUtils.randomChar)

    // Atualiza o tabuleiro com as palavras do arquivo de texto
    board = ZigZagUtils.initializeGameBoardWithWordsFromFile(randomBoard)

    // Preenche novamente os botões da grade com os caracteres do tabuleiro atualizado
    fillButtonsWithBoard(board)

    // Define o número de tentativas como 0
    lblTries.setText("0")
    lblFound.setText("0")
    lblPontos.setText("0")
    enableAllButtons(selectedButtons)
    selectedButtons = Nil
    selectedButtonsCoord = Nil

  }

  private def disableAllButtons(buttons: List[Button]): Unit = buttons match {
    case Nil => // Caso base: lista vazia, não há mais botões para desativar
    case button :: rest =>
      button.setDisable(true) // Desativa o botão atual
      disableAllButtons(rest) // Chama recursivamente a função para os botões restantes
  }

  private def enableAllButtons(buttons: List[Button]): Unit = buttons match {
    case Nil => // Caso base: lista vazia, não há mais botões para ativar
    case button :: rest =>
      button.setDisable(false) // Ativa o botão atual
      button.setStyle("") // Remove qualquer estilo aplicado ao botão
      enableAllButtons(rest) // Chama recursivamente a função para os botões restantes
  }

  private def colorToHex(color: Color): String = {
    val red = (color.getRed * 255).toInt
    val green = (color.getGreen * 255).toInt
    val blue = (color.getBlue * 255).toInt
    f"#$red%02X$green%02X$blue%02X"
  }

  def handleQuitButtonClick(): Unit = {
    Platform.exit()
  }

  def initialize(): Unit = {
    // Inicializa o tabuleiro com letras aleatórias
    val (randomBoard, updatedRandom) = ZigZagUtils.completeBoardRandomly(initialBoard, initialRandom, ZigZagUtils.randomChar)

    // Preencher os botões da grade com os caracteres do tabuleiro aleatório
    fillButtonsWithBoard(randomBoard)

    // Atualizar o tabuleiro com as palavras do arquivo de texto
    board = ZigZagUtils.initializeGameBoardWithWordsFromFile(randomBoard)

    // Preencher novamente os botões da grade com os caracteres do tabuleiro atualizado
    fillButtonsWithBoard(board)
  }



  private def fillButtonsWithBoard(board: Board, row: Int = 0, col: Int = 0): Unit = {
    if (row < gridBoard.getRowCount) {
      if (col < gridBoard.getColumnCount) {
        val button = getButton(row, col)
        val letter = getOneCell(board, (row, col))
        if (letter != '.') { // Verifica se o caractere não é '.' antes de definir o texto do botão
          button.setText(letter.toString)
        }
        fillButtonsWithBoard(board, row, col + 1) // Chama recursivamente para a próxima coluna
      } else {
        fillButtonsWithBoard(board, row + 1, 0) // Passa para a próxima linha e reinicia a contagem de coluna
      }
    }
  }

  private def getButton(row: Int, col: Int): Button = {
    val children = gridBoard.getChildren
    children.get(row * gridBoard.getColumnCount + col).asInstanceOf[Button]
  }

  private def getOneCell(board: Board, coord: Coord2D): Char = {
    val (x, y) = coord
    if (x >= 0 && x < board.length && y >= 0 && y < board(x).length) {
      board(x)(y)
    } else {
      '.'
    }
  }
}
