import ZigZag.{initialBoard, initialRandom}
import ZigZagUtils.{Board, completeBoardRandomly, initializeGameBoardWithWordsFromFile, randomChar}
import javafx.scene.paint.Color
import javafx.scene.control.{Button, TextField}
import javafx.fxml.FXML
import javafx.scene.layout.GridPane
import javafx.event.ActionEvent
import javafx.application
import javafx.application.Platform


class Controller {

  // Referência ao TextField
  var txtWord: TextField = _

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

  // Variáveis de controle
  private var selectedButtons: List[Button] = Nil

  // Cor de fundo para os botões selecionados
  private val selectedButtonColor = Color.DARKORANGE

  // Método para tratamento de cliques nos botões da grade
  def handleButtonClick(event: ActionEvent): Unit = {
    val clickedButton: Button = event.getSource.asInstanceOf[Button]
    val row: Int = GridPane.getRowIndex(clickedButton)
    val col: Int = GridPane.getColumnIndex(clickedButton)

    val button = getButton(row, col)
    if (button.isDisable || selectedButtons.length >= 2) return // Retorna se o botão já estiver desativado ou se já houver dois botões selecionados

    button.setStyle("-fx-background-color: " + colorToHex(selectedButtonColor))
    selectedButtons = button :: selectedButtons
    button.setDisable(true)

    if (selectedButtons.length == 2) {
      // Desativa todos os botões
      disableAllButtons()


    }
  }

  // Método para tratar cliques no botão "Play"
  def handlePlayButtonClick(): Unit = {
    // Realiza a pesquisa da palavra (a ser implementado)
    // Após a pesquisa, reativa os botões e limpa a lista de botões selecionados
    enableAllButtons()
    selectedButtons = Nil
  }



  // Método para desativar todos os botões
  private def disableAllButtons(): Unit = {
    selectedButtons.foreach { button =>
      button.setDisable(true)
    }
  }

  // Método para reativar todos os botões
  private def enableAllButtons(): Unit = {
    selectedButtons.foreach { button =>
      button.setDisable(false)
      button.setStyle("") // Remove o estilo de fundo
    }
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

  }
}
