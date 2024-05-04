import ZigZag.{initialBoard, initialRandom}
import ZigZagUtils.{completeBoardRandomly, initializeGameBoardWithWordsFromFile, randomChar}
import javafx.scene.paint.Color
import javafx.scene.control.Button
import javafx.fxml.FXML
import javafx.scene.layout.GridPane
import javafx.event.ActionEvent
import javafx.application
import javafx.application.Platform


class Controller {

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

  // Define a cor de fundo para os botões selecionados
  private val selectedButtonColor = Color.DARKORANGE

  // Método para tratamento de cliques nos botões da grade
  def handleButtonClick(event: ActionEvent): Unit = {
    val clickedButton: Button = event.getSource.asInstanceOf[Button] // Obtém o botão clicado
    val row: Int = GridPane.getRowIndex(clickedButton) // Obtém a linha do botão
    val col: Int = GridPane.getColumnIndex(clickedButton) // Obtém a coluna do botão

    val button = getButton(row, col)
    if (button.isDisable) return

    button.setStyle("-fx-background-color: " + colorToHex(selectedButtonColor))
    selectedButtons = button :: selectedButtons
    button.setDisable(true)
  }

  // Método para tratamento de cliques no botão "Play"
  def handlePlayButtonClick(): Unit = {
    // Lógica para verificar se a palavra está correta
    // e atualizar os botões conforme necessário

    selectedButtons.foreach { button =>

      button.getStyleClass.add("btn-label")
      button.setDisable(false) // Reativa o botão
    }
    selectedButtons = Nil
  }

  private def getButton(row: Int, col: Int): Button = {
    val colIndex = col
    val rowIndex = row
    val buttonId = s"btn$rowIndex$colIndex"
    val field = getClass.getDeclaredField(buttonId)
    field.setAccessible(true)
    field.get(this).asInstanceOf[Button]
  }

  // Converte um objeto Color para uma string hexadecimal
  private def colorToHex(color: Color): String = {
    val red = (color.getRed * 255).toInt
    val green = (color.getGreen * 255).toInt
    val blue = (color.getBlue * 255).toInt
    f"#$red%02X$green%02X$blue%02X"
  }


  def handleQuitButtonClick(): Unit = {
    Platform.exit()
  }

  def fillButtonsRandomly(): Unit = {


  }


  def initialize(): Unit = {
    fillButtonsRandomly()
  }
}
