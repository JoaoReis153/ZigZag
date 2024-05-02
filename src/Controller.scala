
import javafx.collections.FXCollections
import javafx.fxml.FXML
import javafx.scene.control.{Button, ChoiceBox, Label, TableColumn, TableRow, TableView, TextField}


class Controller {

  //ChoiceBox da Direction:
  @FXML
  private var cboxDirection: ChoiceBox[String] = _

  //Valores do histórico:
  @FXML
  private var numberGame: TextField = _
  private var numberTries: TextField = _
  private var numberFound: TextField = _

  //Letras do ZigZag :
  @FXML private var lbl00: Label = _
  @FXML private var lbl01: Label = _
  @FXML private var lbl02: Label = _
  @FXML private var lbl03: Label = _
  @FXML private var lbl04: Label = _
  @FXML private var lbl10: Label = _
  @FXML private var lbl11: Label = _
  @FXML private var lbl12: Label = _
  @FXML private var lbl13: Label = _
  @FXML private var lbl14: Label = _
  @FXML private var lbl20: Label = _
  @FXML private var lbl21: Label = _
  @FXML private var lbl22: Label = _
  @FXML private var lbl23: Label = _
  @FXML private var lbl24: Label = _
  @FXML private var lbl30: Label = _
  @FXML private var lbl31: Label = _
  @FXML private var lbl32: Label = _
  @FXML private var lbl33: Label = _
  @FXML private var lbl34: Label = _
  @FXML private var lbl40: Label = _
  @FXML private var lbl41: Label = _
  @FXML private var lbl42: Label = _
  @FXML private var lbl43: Label = _
  @FXML private var lbl44: Label = _

  //Textfields :
  @FXML
  private var txtCoordRow: TextField = _
  @FXML
  private var txtCoordColumn: TextField = _
  @FXML
  private var txtWord: TextField = _


  //botoes :
  @FXML
  private var btnSearch: Button = _
  @FXML
  private var btnNewGame: Button = _
  @FXML
  private var btnQuit: Button = _


  // Função auxiliar para incrementar um contador
  def incrementTextField(textField: TextField): Unit = {
    val currentValue = textField.getText.toInt
    val newValue = currentValue + 1
    textField.setText(newValue.toString)
  }

  // Métodos de evento para os botões

  // Evento de clique do botão de pesquisa
  @FXML
  def OnbtnSearchClick(): Unit = {
    try {
      val x = txtCoordRow.getText.toInt
      val y = txtCoordColumn.getText.toInt
      val directionStr = cboxDirection.getValue.toUpperCase
      Direction.stringToDirection(directionStr) match {
        case Some(direction) =>
          if (play(initialState.board, txtWord.getText.toUpperCase, (y, x), direction)) {
            incrementTextField(numberFound)
          } else {
            println("Tenta outra vez!")
          }
          incrementTextField(numberTries)
        case None => println("Direção inválida")
      }
    } catch {
      case _: NumberFormatException => println("Input inválido")
    }
  }

  @FXML
  def OnbtnNewGameClick(): Unit = {
    initialize()

  }

  @FXML
  def OnbtnQuitClick(): Unit = {
    val stage: Stage = numberGame.getScene.getWindow.asInstanceOf[Stage]
    stage.close()

  }




  def initialize(): Unit = {
    // Define as condições de inicio
    numberGame.setText("1")
    numberTries.setText("0")
    numberFound.setText("0")
    cboxDirection.setItems(FXCollections.observableArrayList("North", "South", "East", "West", "NorthWest", "SouthWest", "Northeast", "Southeast"))
    
  }
}