import javafx.collections.FXCollections
import javafx.fxml.FXML
import javafx.scene.control.{Button, ChoiceBox, Label, TableView, TextField}


class Controller {

  //ChoiceBox da Direction:
  @FXML
  private var cboxDirection: ChoiceBox[String] = _


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

  // Métodos de evento para os botões
  @FXML
  def OnbtnSearchClick(): Unit = {
    // Lógica para o evento de clique do botão de pesquisa
  }

  @FXML
  def OnbtnNewGameClick(): Unit = {
    // Lógica para o evento de clique do botão de novo jogo
  }

  @FXML
  def OnbtnQuitClick(): Unit = {
    // Lógica para o evento de clique do botão de sair
  }




  def initialize(): Unit = {
    // Define as opções da ChoiceBox
    cboxDirection.setItems(FXCollections.observableArrayList("North", "South", "East", "West", "NorthWest", "SouthWest", "Northeast", "Southeast"))
  }
}