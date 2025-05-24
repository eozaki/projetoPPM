import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control.{Button, Slider, TextField}
import javafx.scene.input.MouseEvent
import javafx.scene.shape.Circle
import Game._
import TUI._
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.scene.paint.Color


class GUI {

  var inGame: Int = 0

  private var currentState: Option[GameState] = None
  private var gameHistory: GameHistory = List()

  @FXML  private var btn_novoJogo: Button = _
  @FXML  private var btn_carregJogo: Button = _
  @FXML  private var btn_undoJogada: Button = _
  @FXML  private var btn_guardarJogo: Button = _
  @FXML  private var btn_sair: Button = _
  @FXML  private var tf_tempMaxJog: TextField = _
  @FXML  private var tf_pecCapt: TextField = _
  @FXML  private var tf_nomeficheiro: TextField =_
  @FXML  private var sld_dific: Slider = _
  @FXML  private var _1_1: Circle = _
  @FXML  private var _2_1: Circle = _
  @FXML  private var _3_1: Circle = _
  @FXML  private var _4_1: Circle = _
  @FXML  private var _5_1: Circle = _
  @FXML  private var _1_2: Circle = _
  @FXML  private var _2_2: Circle = _
  @FXML  private var _3_2: Circle = _
  @FXML  private var _4_2: Circle = _
  @FXML  private var _5_2: Circle = _
  @FXML  private var _1_3: Circle = _
  @FXML  private var _2_3: Circle = _
  @FXML  private var _3_3: Circle = _
  @FXML  private var _4_3: Circle = _
  @FXML  private var _5_3: Circle = _
  @FXML  private var _1_4: Circle = _
  @FXML  private var _2_4: Circle = _
  @FXML  private var _3_4: Circle = _
  @FXML  private var _4_4: Circle = _
  @FXML  private var _5_4: Circle = _
  @FXML  private var _1_5: Circle = _
  @FXML  private var _2_5: Circle = _
  @FXML  private var _3_5: Circle = _
  @FXML  private var _4_5: Circle = _
  @FXML  private var _5_5: Circle = _

  private val circleMap = scala.collection.mutable.Map[Coord2D, Circle]()

  @FXML
  def initialize(): Unit = {
    val coords = for (r <- 1 to 5; c <- 1 to 5) yield (r, c)
    val circles = List(
      _1_1, _1_2, _1_3, _1_4, _1_5,
      _2_1, _2_2, _2_3, _2_4, _2_5,
      _3_1, _3_2, _3_3, _3_4, _3_5,
      _4_1, _4_2, _4_3, _4_4, _4_5,
      _5_1, _5_2, _5_3, _5_4, _5_5
    )

    coords.zip(circles).foreach {
      case (coord, circle) =>
        circle.setOpacity(0.0)
        circleMap += (coord -> circle)
        addCircleClickHandler(coord, circle)
        addHoverEffect(circle)
    }
  }

  private def addHoverEffect(circle: Circle): Unit = {
    circle.setOnMouseEntered((_: MouseEvent) => {
      if (currentState.nonEmpty && circle.getOpacity == 0.0)
        circle.setOpacity(0.5)
    })
    circle.setOnMouseExited((_: MouseEvent) => {
      if (currentState.nonEmpty && circle.getOpacity == 0.5)
        circle.setOpacity(0.0)
    })
  }

  private def addCircleClickHandler(coord: Coord2D, circle: Circle): Unit = {
    circle.setOnMouseClicked((_: MouseEvent) => {
      currentState match {
        case Some(state) if state.currentPlayer == Stone.Black =>
          // guarda antes da jogada
          gameHistory = state :: gameHistory
          val (nextState, maybeVictory) = makeMove(state, coord, 3)
          currentState = Some(nextState)
          atualizarTabuleiro(nextState)

          maybeVictory match {
            case Some(msg) => mostrarPopup(msg)
            case None => jogarComputador(nextState)
          }

        case _ =>
      }
    })
  }

  private def jogarComputador(state: GameState): Unit = {
    // guarda antes da jogada
    gameHistory = state :: gameHistory

    val (coordBot, _) = randomMove(state.openCoords, MyRandom(System.currentTimeMillis()))
    val (newState, maybeVictory) = makeMove(state, coordBot, 3)

    currentState = Some(newState)
    atualizarTabuleiro(newState)
    maybeVictory.foreach(mostrarPopup)
  }

  private def atualizarTabuleiro(state: GameState): Unit = {
    state.board.zipWithIndex.foreach {
      case (row, i) =>
        row.zipWithIndex.foreach {
          case (stone, j) =>
            val coord = (i + 1, j + 1)
            val circle = circleMap(coord)
            stone match {
              case Stone.Black =>
                circle.setOpacity(1.0)
                circle.setFill(Color.BLACK)
              case Stone.White =>
                circle.setOpacity(1.0)
                circle.setFill(Color.WHITE)
              case Stone.Empty =>
                circle.setOpacity(0.0)
                circle.setFill(Color.TRANSPARENT)
            }
        }
    }
  }

  @FXML
  def btn_novoJogoClicked(): Unit = {
    val captureGoal = tf_pecCapt.getText.trim.toIntOption.getOrElse(3)
    val timeLimitMillis = tf_tempMaxJog.getText.trim.toIntOption.getOrElse(10000)
    val dificuldade = if (sld_dific.getValue <= 3) "facil"
    else if (sld_dific.getValue >= 7) "dificil"
    else "media"

    val settings = Settings(5, 5, captureGoal, timeLimitMillis, dificuldade)
    val newState = criarEstadoInicial(settings)
    currentState = Some(newState)
    gameHistory = List()
    atualizarTabuleiro(newState)
  }

  @FXML
  def btn_undoJogadaClicked(): Unit = {
    gameHistory match {
      case prev :: rest =>
        currentState = Some(prev)
        gameHistory = rest
        atualizarTabuleiro(prev)
      case Nil =>
        mostrarPopup("Não há jogadas para desfazer.")
    }
  }

  @FXML
  def btn_guardarJogoClicked(): Unit = {
    val nome = tf_nomeficheiro.getText.trim
    currentState match {
      case Some(state) if nome.nonEmpty =>
        guardarJogo(state, nome)
        mostrarPopup("Jogo guardado com sucesso!")
      case Some(_) =>
        mostrarPopup("Nome do ficheiro está vazio.")
      case None =>
        mostrarPopup("Nenhum estado de jogo presente para guardar.")
    }
    tf_nomeficheiro.setText("")
  }

  @FXML
  def btn_carregJogoClicked(): Unit = {
    val nome = tf_nomeficheiro.getText.trim
    if (nome.isEmpty) {
      mostrarPopup("Insira o nome do ficheiro para carregar.")
      return
    }

    carregarJogo(nome) match {
      case Some(state) =>
        currentState = Some(state)
        gameHistory = List()
        atualizarTabuleiro(state)
        mostrarPopup("Jogo carregado com sucesso.")
      case None =>
        mostrarPopup("Erro ao carregar o jogo. Verifique o nome do ficheiro.")
    }
  }

  @FXML
  def btn_reiniciarClicked(): Unit = {
    currentState match {
      case Some(state) =>
        val width = state.board.head.length
        val height = state.board.length
        val captureGoal = tf_pecCapt.getText.trim.toIntOption.getOrElse(3)
        val timeLimit = tf_tempMaxJog.getText.trim.toIntOption.getOrElse(10000)
        val dificuldade =
          if (sld_dific.getValue <= 3) "facil"
          else if (sld_dific.getValue >= 7) "dificil"
          else "media"

        val settings = Settings(width, height, captureGoal, timeLimit, dificuldade)
        val newState = criarEstadoInicial(settings)

        currentState = Some(newState)
        gameHistory = List()
        atualizarTabuleiro(newState)
        mostrarPopup("Jogo reiniciado.")

      case None =>
        mostrarPopup("Não há jogo ativo para reiniciar.")
    }
  }

  @FXML
  def btn_sairClicked(): Unit = {
    Platform.exit()
  }

  private def mostrarPopup(msg: String): Unit = {
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle("Atari Go")
    alert.setHeaderText(null)
    alert.setContentText(msg)
    alert.showAndWait()
  }

}
