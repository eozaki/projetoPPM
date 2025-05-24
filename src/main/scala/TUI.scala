import Game._
import scala.io.StdIn.readLine

object TUI {

  case class Settings(boardWidth: Int, boardHeight: Int, captureGoal: Int, timeLimitMillis: Long, difficulty: String)

  def start(): Unit = {
    val settings = Settings(5, 5, 3, 10000L, "facil")
    val initialState = criarEstadoInicial(settings)
    mainMenu(settings, List(initialState))
  }

  @scala.annotation.tailrec
  def mainMenu(settings: Settings, history: List[GameState]): Unit = {
    println("\n--- Atari Go ---")
    println("1. Novo jogo")
    println("2. Alterar dimensões do tabuleiro")
    println("3. Definir número de peças a capturar")
    println("4. Definir tempo máximo por jogada")
    println("5. Definir nível de dificuldade")
    println("6. Carregar jogo")
    println("0. Sair")

    readLine("Escolha uma opção: ") match {
      case "1" => jogoMenu(settings, history)
      case "2" =>
        val (w, h) = lerDimensoes()
        val newSettings = settings.copy(boardWidth = w, boardHeight = h)
        val newState = criarEstadoInicial(newSettings)
        mainMenu(newSettings, List(newState))
      case "3" =>
        val n = lerNumero("Número de capturas para vencer: ")
        mainMenu(settings.copy(captureGoal = n), history)
      case "4" =>
        val t = lerNumero("Tempo máximo por jogada (ms): ")
        mainMenu(settings.copy(timeLimitMillis = t.toLong), history)
      case "5" =>
        val d = readLine("Dificuldade (facil / dificil): ")
        mainMenu(settings.copy(difficulty = d), history)
      case "6" =>
        val nome = readLine("Nome do ficheiro (sem .txt): ").trim
        carregarJogo(nome) match {
          case Some(state) =>
            println("Jogo carregado com sucesso!\n")
            Game.displayBoard(state.board)
            println()
            jogoMenu(settings, List(state))
          case None =>
            println("Erro ao carregar jogo!")
            mainMenu(settings, history)
        }
      case "0" =>
        println("A sair do jogo. Até breve!")
      case _ =>
        println("Opção inválida.")
        mainMenu(settings, history)
    }
  }

  @scala.annotation.tailrec
  def jogoMenu(settings: Settings, history: List[GameState]): Unit = {
    //val state = history.head
    //Game.displayBoard(state.board)
    //println()
    println("Turno: Jogador (preto)")
    println("1. Jogar")
    println("2. Undo")
    println("3. Guardar jogo")
    println("4. Reiniciar")
    println("0. Sair")

    readLine("Escolha uma opção: ") match {
      case "1" => jogarTurnoJogador(settings, history)
      case "2" =>
        val (maybePrev, newHist) = undo(history)
        maybePrev match {
          case Some(state) =>
            println("Última jogada anulada.")
            Game.displayBoard(newHist.head.board)
            println()
            jogoMenu(settings, newHist)
          case None =>
            println("Não é possível anular.")
            jogoMenu(settings, history)
        }
      case "3" =>
        val nome = readLine("Nome do ficheiro (sem .txt): ").trim
        guardarJogo(history.head, nome)
        jogoMenu(settings, history)
      case "4" =>
        val newState = criarEstadoInicial(settings)
        mainMenu(settings, List(newState))
      case "0" =>
        println("A voltar ao menu principal.")
        mainMenu(settings, history)
      case _ =>
        println("Opção inválida.")
        jogoMenu(settings, history)
    }
  }

  def jogarTurnoJogador(settings: Settings, history: List[GameState]): Unit = {
    val state = history.head

    println(s"Jogador (preto), introduz a linha e a coluna (tens ${settings.timeLimitMillis / 1000} segundos):")
    val startTime = System.currentTimeMillis()
    val input = readLine().split(" ").map(_.toInt)
    val endTime = System.currentTimeMillis()

    if (isTimeExceeded(startTime, endTime, settings.timeLimitMillis)) {
      println("⏱️ Tempo esgotado! Jogada perdida.")
      val skipped = state.copy(currentPlayer = Stone.White)
      jogarComputador(settings, skipped :: history)
    } else {
      val coord = (input(0), input(1))
      val (newState, maybeVictory) = makeMove(state, coord, settings.captureGoal)
      maybeVictory match {
        case Some(msg) =>
          Game.displayBoard(newState.board)
          println(s"🏁 $msg")
          mainMenu(settings, newState :: history)
        case None =>
          jogarComputador(settings, newState :: history)
      }
    }
  }

  def jogarComputador(settings: Settings, history: List[GameState]): Unit = {
    val state = history.head
    val rand = MyRandom(System.currentTimeMillis())

    println("Computador (branco) a jogar...")
    val (coord, newRand) = randomMove(state.openCoords, rand)
    val (newState, maybeVictory) = makeMove(state, coord, settings.captureGoal)

    maybeVictory match {
      case Some(msg) =>
        //Game.displayBoard(newState.board)
        //1println(s"🏁 $msg")
        mainMenu(settings, newState :: history)
      case None =>
        jogoMenu(settings, newState :: history)
    }
    Game.displayBoard(newState.board)
    println()
    jogoMenu(settings, newState :: history)
  }

  def criarEstadoInicial(settings: Settings): GameState = {
    val board = populateBoard(settings.boardHeight, settings.boardWidth)
    val coords = populateRows(settings.boardHeight, settings.boardWidth)
    GameState(board, coords, Stone.Black, 0, 0)
  }

  def lerNumero(msg: String): Int = {
    print(msg)
    scala.io.StdIn.readInt()
  }

  def lerDimensoes(): (Int, Int) = {
    val w = lerNumero("Largura do tabuleiro: ")
    val h = lerNumero("Altura do tabuleiro: ")
    (w, h)
  }

  def guardarJogo(state: GameState, nomeFicheiro: String): Unit = {
    val path = s"$nomeFicheiro.txt"
    val fileContent = new StringBuilder

    // Estado do tabuleiro
    fileContent.append("TABULEIRO:\n")
    state.board.foreach { row =>
      fileContent.append(row.map {
        case Stone.Black => "B"
        case Stone.White => "W"
        case Stone.Empty => "."
      }.mkString("")).append("\n")
    }

    // Coordenadas livres
    fileContent.append("LIVRES:\n")
    state.openCoords.foreach { case (r, c) => fileContent.append(s"$r,$c\n") }

    // Jogador atual
    fileContent.append("JOGADOR:\n")
    fileContent.append(state.currentPlayer.toString + "\n")

    // Capturas
    fileContent.append("CAPTURAS:\n")
    fileContent.append(s"${state.playerCaptured},${state.computerCaptured}\n")

    import java.io.PrintWriter
    val writer = new PrintWriter(path)
    writer.write(fileContent.toString)
    writer.close()

    println("Jogo guardado com sucesso!")

  }

  def carregarJogo(nomeFicheiro: String): Option[GameState] = {
    val path = s"$nomeFicheiro.txt"
    val source = scala.util.Try(scala.io.Source.fromFile(path))

    source.toOption match {
      case None => None
      case Some(file) =>
        val lines = file.getLines().toList.map(_.trim)
        file.close()

        try {
          val marcadores = Set("TABULEIRO:", "LIVRES:", "JOGADOR:", "CAPTURAS:")

          val tabuleiroRaw = extrairBloco(lines, "TABULEIRO:", marcadores)
          val board = tabuleiroRaw.map { line =>
            line.map {
              case 'B' => Stone.Black
              case 'W' => Stone.White
              case '.' => Stone.Empty
              case other => throw new Exception(s"Caractere inválido no tabuleiro: '$other'")
            }.toList
          }

          val livresRaw = extrairBloco(lines, "LIVRES:", marcadores)
          val coords = livresRaw.map { line =>
            val Array(r, c) = line.split(",").map(_.trim.toInt)
            (r, c)
          }

          val jogador = lines.dropWhile(_ != "JOGADOR:").drop(1).head.trim match {
            case "Black" => Stone.Black
            case "White" => Stone.White
            case _       => throw new Exception("Jogador inválido.")
          }

          val Array(pCapt, cCapt) = lines.dropWhile(_ != "CAPTURAS:").drop(1).head.split(",").map(_.trim.toInt)

          Some(GameState(board, coords, jogador, pCapt, cCapt))
        } catch {
          case e: Throwable =>
            println(s"Erro ao carregar: ${e.getMessage}")
            None
        }
    }
  }

  def extrairBloco(lines: List[String], inicio: String, fim: Set[String]): List[String] = {
    lines.dropWhile(_ != inicio).drop(1).takeWhile(line => !fim.contains(line.trim))
  }

}