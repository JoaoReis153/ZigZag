import ZigZagUtils.{Board, Direction, completeBoardRandomly, getWordList, initializeGameBoardWithWordsFromFile, play}
import scala.collection.mutable
import javax.swing.{BorderFactory, Box, BoxLayout, JButton, JComboBox, JComponent, JFrame, JLabel, JPanel, JTextField, SwingConstants, SwingUtilities, WindowConstants}
import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, GridLayout}

object ZigZagGUI {

  private val initialBoard: Board = List.fill(5)(List.fill(5)('.'))
  private var random: MyRandom        = MyRandom(System.currentTimeMillis())
  private var board: Board            = initialBoard
  private var numTries: Int           = 0
  private var numFound: Int           = 0
  private val foundWords              = mutable.Set[String]()
  private var totalWords: Int         = 0
  private var playBtn: JButton        = _

  private val cells       = Array.ofDim[JLabel](5, 5)
  private val triesLabel  = new JLabel("Tries: 0")
  private val foundLabel  = new JLabel("Found: 0")
  private val statusLabel = new JLabel("Find the hidden words!", SwingConstants.CENTER)

  def main(args: Array[String]): Unit =
    SwingUtilities.invokeLater(() => {
      newGame()
      val frame = buildFrame()
      updateBoard()
      updateStats()
      frame.pack()
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
    })

  private def newGame(): Unit = {
    random = MyRandom(System.currentTimeMillis())
    val (filled, nextRandom) = completeBoardRandomly(initialBoard, random, ZigZagUtils.randomChar)
    random     = nextRandom
    board      = initializeGameBoardWithWordsFromFile(filled)
    numTries   = 0
    numFound   = 0
    totalWords = getWordList().length
    foundWords.clear()
    if (playBtn != null) playBtn.setEnabled(true)
    updateBoard()
    updateStats()
    setStatus(s"Find all $totalWords hidden words!", Color.DARK_GRAY)
  }

  private def updateBoard(): Unit =
    for (r <- 0 until 5; c <- 0 until 5)
      if (cells(r)(c) != null) cells(r)(c).setText(board(r)(c).toString)

  private def updateStats(): Unit = {
    triesLabel.setText(s"Tries: $numTries")
    foundLabel.setText(s"Found: $numFound")
  }

  private def setStatus(msg: String, color: Color): Unit = {
    statusLabel.setText(msg)
    statusLabel.setForeground(color)
  }

  // ── UI construction ────────────────────────────────────────────────────────

  private def buildFrame(): JFrame = {
    val frame = new JFrame("ZigZag — Word Search")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setLayout(new BorderLayout(10, 10))
    frame.add(buildBoardPanel(),  BorderLayout.CENTER)
    frame.add(buildInputPanel(),  BorderLayout.EAST)
    frame.add(buildStatsPanel(),  BorderLayout.NORTH)
    frame.add(statusLabel,        BorderLayout.SOUTH)
    statusLabel.setBorder(BorderFactory.createEmptyBorder(4, 10, 8, 10))
    statusLabel.setFont(new Font("SansSerif", Font.PLAIN, 13))
    frame
  }

  private def buildBoardPanel(): JPanel = {
    val panel = new JPanel(new GridLayout(5, 5, 4, 4))
    panel.setBorder(BorderFactory.createEmptyBorder(12, 12, 12, 12))
    panel.setBackground(new Color(60, 60, 60))
    for (r <- 0 until 5; c <- 0 until 5) {
      val cell = new JLabel(".", SwingConstants.CENTER)
      cell.setFont(new Font("Monospaced", Font.BOLD, 28))
      cell.setOpaque(true)
      cell.setBackground(Color.WHITE)
      cell.setPreferredSize(new Dimension(60, 60))
      cells(r)(c) = cell
      panel.add(cell)
    }
    panel
  }

  private def buildInputPanel(): JPanel = {
    val wordField = new JTextField(10)
    val rowField  = new JTextField(3)
    val colField  = new JTextField(3)
    val dirCombo  = new JComboBox[String](Array(
      "North", "NorthEast", "East", "SouthEast",
      "South", "SouthWest", "West", "NorthWest"
    ))

    playBtn        = new JButton("▶  Play")
    val newGameBtn = new JButton("↺  New Game")

    playBtn.addActionListener(_ => handlePlay(wordField, rowField, colField, dirCombo))
    newGameBtn.addActionListener(_ => newGame())

    val panel = new JPanel()
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))
    panel.setBorder(BorderFactory.createTitledBorder("Guess a Word"))
    panel.add(row("Word:",       wordField))
    panel.add(Box.createVerticalStrut(6))
    panel.add(row("Row (0–4):", rowField))
    panel.add(Box.createVerticalStrut(6))
    panel.add(row("Col (0–4):", colField))
    panel.add(Box.createVerticalStrut(6))
    panel.add(row("Direction:",  dirCombo))
    panel.add(Box.createVerticalStrut(12))
    val btnRow = new JPanel(new FlowLayout())
    btnRow.add(playBtn)
    btnRow.add(newGameBtn)
    panel.add(btnRow)
    panel
  }

  private def buildStatsPanel(): JPanel = {
    val panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 6))
    Seq(triesLabel, foundLabel).foreach { l =>
      l.setFont(new Font("SansSerif", Font.BOLD, 14))
      panel.add(l)
    }
    panel
  }

  private def row(label: String, comp: JComponent): JPanel = {
    val p = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0))
    val l = new JLabel(label)
    l.setPreferredSize(new Dimension(90, 24))
    p.add(l)
    p.add(comp)
    p
  }

  // ── Game logic ─────────────────────────────────────────────────────────────

  private def handlePlay(
    wordField: JTextField,
    rowField:  JTextField,
    colField:  JTextField,
    dirCombo:  JComboBox[String]
  ): Unit = {
    val word   = wordField.getText.trim.toUpperCase
    val dirStr = dirCombo.getSelectedItem.asInstanceOf[String].toUpperCase
    if (word.isEmpty) { setStatus("Enter a word.", Color.RED); return }
    try {
      val r = rowField.getText.trim.toInt
      val c = colField.getText.trim.toInt
      Direction.stringToDirection(dirStr) match {
        case Some(dir) =>
          numTries += 1
          if (play(board, word, (r, c), dir)) {
            if (foundWords.contains(word)) {
              setStatus(s"'$word' was already found!", new Color(200, 120, 0))
            } else {
              foundWords += word
              numFound   += 1
              updateStats()
              if (numFound == totalWords) {
                setStatus(s"You found all $totalWords words in $numTries tries! Press New Game to play again.", new Color(0, 140, 0))
                playBtn.setEnabled(false)
              } else {
                setStatus(s"'$word' found! ${totalWords - numFound} word(s) remaining.", new Color(0, 140, 0))
              }
            }
          } else {
            setStatus(s"'$word' not found there. Try again!", Color.RED)
          }
          updateStats()
        case None =>
          setStatus("Invalid direction.", Color.RED)
      }
    } catch {
      case _: NumberFormatException =>
        setStatus("Row and column must be numbers (0–4).", Color.RED)
    }
  }
}
