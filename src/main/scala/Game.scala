import Game.{Board, Coord2D, MyRandom, Stone, playRandomly, populateBoard, populateRows, randomMove}
import Game._
import Game.Stone.Stone
import scala.annotation.tailrec

case class Game(board: Board, coords: List[Coord2D]) {
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]):(Option[Board], List[Coord2D]) =
    Game.play(board, player, coord, lstOpenCoords)
}

object Game {
  object Stone extends Enumeration {
    type Stone = Value
    val Black, White, Empty = Value
  }

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) // (row, column)

  // T2
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = (coord, lstOpenCoords) match {
    case (c, l) if validCoord(c, l) => {
      val newBoard = playRow(c._1, c._2, player, board)
      val l = remainingCoords(lstOpenCoords, coord)
      displayBoard(newBoard) // n ter aña mm funçao
      println(l)
      (Some(newBoard), l)
    }
    case (c, l) if !validCoord(c, l) => (None, lstOpenCoords)
  }

  private def remainingCoords(l: List[Coord2D], coord: Coord2D): List[Coord2D] = {
    l match {
      case List() => List()
      case (x, y) :: xs if x == coord._1 && y == coord._2 => xs
      case (x, y) :: xs if x != coord._1 || y != coord._2 => (x, y) :: remainingCoords(xs, coord)
    }
  }

  private def playRow(row: Int, col: Int, player: Stone, board: Board): Board = (row, col, board) match {
    case (_, _, List()) => List()
    case (1, col, x :: xs) => playCol(col, x, player) :: xs
    case (n, col, x :: xs) => x :: playRow(n - 1, col, player, xs)
  }

  private def playCol(col: Int, list: List[Game.Stone.Stone], player: Game.Stone.Stone): List[Game.Stone.Stone] = (col, list) match {
    case (_, List()) => List()
    case (1, _ :: xs) => player :: xs
    case (n, x :: xs) => x :: playCol(n - 1, xs, player)
  }

  // não há necessidade de utilizar recursividade - há outras implementações em scala para isso

  @tailrec
  private def validCoord(coord: Coord2D, coordList: List[Coord2D]): Boolean = coordList match {
    case List() => false
    case x :: _ if x == coord => true
    case _ :: xs => validCoord(coord, xs)
  }

  def populateBoard(width: Int, height: Int): Board = (width, height) match {
    case (0, _) => List()
    case (row, column) => populateBoardColumn(row, column) :: populateBoard(row - 1, column)
  }

  private def populateBoardColumn(fixed: Int, changing: Int): List[Game.Stone.Stone] = (fixed, changing) match {
    case (_, 0) => List()
    case (_row, column) => Stone.Empty :: populateBoardColumn(_row, column - 1)
  }

  def populateRows(width: Int, height: Int): List[Coord2D] = (width, height) match {
    case (0, _) => List()
    case (row, column) => populateColumns(row, column) ++ populateRows(row - 1, column)
  }

  private def populateColumns(fixed: Int, changing: Int): List[Coord2D] = (fixed, changing) match {
    case (_, 0) => List()
    case (row, column) => (row, column) :: populateColumns(row, column - 1)
  }

  @tailrec
  def displayBoard(board: Board): Unit = board match {
    case Nil => ()
    case row :: tail =>
      displayRow(row)
      println()
      displayBoard(tail)
  }

  @tailrec
  private def displayRow(row: List[Stone]): Unit = row match {
    case Nil => ()
    case head :: tail =>
      head match {
        case Stone.Black => print("B ")
        case Stone.White => print("W ")
        case Stone.Empty => print(". ")
      }

      displayRow(tail)
  }

  // T1
  trait Random {
    def nextInt(x: Int): (Int, Random)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, MyRandom) = { // recebe um Int e devolve in Int e um random
      val newSeed = (seed * 0x11111111L + 0xBL) & 0xFFFFFFFFFFFFL // atualiza a seed com um novo valor
      val nextRandom = MyRandom(newSeed) // cria um novo MyRandom com a nova seed
      val n = (((newSeed >>> 16).toInt)) % x // gera um x aleatório [0, n-1]
      (if (n < 0) -n else n, nextRandom) // garante que o x está entre [0, n-1]
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = lstOpenCoords match {
    case List() => throw new IllegalArgumentException("Não há coordenadas livres disponíveis.") // caso a lista esteja vazia
    case list =>
      val (index, newRand) = rand.nextInt(list.size) // Gera um índice aleatório com base no tamanho da lista
      val chosenCoord = list(index) // Acede à coordenada da lista dada por index
      (chosenCoord, newRand) // Retorna a coord e nova seed para números aleatórios
  }


  // T3
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {

    val (coordAleat, novaSeed) = f(lstOpenCoords, r)
    val (updatedBoard, updatedOpenCoords: List[Coord2D]) = play(board, player, coordAleat, lstOpenCoords)

    updatedBoard match {
      case Some(updatedBoard) => (updatedBoard, novaSeed, updatedOpenCoords)
      case None => (board, novaSeed, lstOpenCoords)
    }
  }

  // T5 ERICK
  def captureGroupStones(board: Board, player: Stone, playerCaptured: Int, computerCaptured: Int): (Board, Int, Int) = {
    val adversary = if (player == Stone.Black) Stone.White else Stone.Black

    @scala.annotation.tailrec
    def capturar(coords: List[Coord2D], visitados: Set[Coord2D], tab: Board, totalCapturado: Int): (Board, Int) = coords match {
      case Nil => (tab, totalCapturado)
      case coord :: resto =>
        if (visitados.contains(coord) || getStone(tab, coord) != adversary)
          capturar(resto, visitados, tab, totalCapturado)
        else {
          val grupo = grupoLigado(coord, tab, adversary, Set())
          val novoVisitados = visitados ++ grupo

          if (!temLiberdades(grupo, tab)) {
            val novoTab = removerGrupo(grupo, tab)
            capturar(resto, novoVisitados, novoTab, totalCapturado + grupo.size)
          } else {
            capturar(resto, novoVisitados, tab, totalCapturado)
          }
        }
    }

    val allCoords = for {
      row <- board.indices
      col <- board.head.indices
    } yield (row + 1, col + 1) // +1 porque o jogo parece usar índices a partir de 1

    val (newBoard, capturedNow) = capturar(allCoords.toList, Set(), board, 0)
    val updatedPlayerCaptured = if (player == Stone.Black)
      playerCaptured + capturedNow
    else
      playerCaptured

    val updatedComputerCaptured = if (player == Stone.White)
      computerCaptured + capturedNow
    else computerCaptured

    (newBoard, updatedPlayerCaptured, updatedComputerCaptured)
  }

  // Funções Auxiliares Necessárias:
  // adjacentes(coord: Coord2D) → devolve as 4 coordenadas ortogonais adjacentes.
  // grupoLigado(coord, board, jogador) → encontra todas as pedras conectadas (DFS ou BFS).
  // temLiberdades(grupo, board) → verifica se o grupo tem posições Empty adjacentes.
  // removerGrupo(grupo, board) → substitui as coordenadas do grupo por Empty.

  private def getStone(board: Board, coord: Coord2D): Stone =
    board(coord._1 - 1)(coord._2 - 1)

  private def adjacentes(coord: Coord2D): List[Coord2D] = {
    val (r, c) = coord
    List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
  }

  //@scala.annotation.tailrec
  private def grupoLigado(coord: Coord2D, board: Board, stone: Stone, visitados: Set[Coord2D], acc: Set[Coord2D] = Set()): Set[Coord2D] = {
    val novos = adjacentes(coord)
      .filter(c => !visitados.contains(c) && dentroDoTabuleiro(c, board) && getStone(board, c) == stone)

    if (novos.isEmpty) acc + coord
    else novos.foldLeft(acc + coord) { (a, novoCoord) =>
      grupoLigado(novoCoord, board, stone, visitados ++ acc, a)
    }
  }

  private def temLiberdades(grupo: Set[Coord2D], board: Board): Boolean =
    grupo.exists(coord => adjacentes(coord).exists(c =>
      dentroDoTabuleiro(c, board) && getStone(board, c) == Stone.Empty))

  private def removerGrupo(grupo: Set[Coord2D], board: Board): Board = {
    board.zipWithIndex.map { case (linha, r) =>
      linha.zipWithIndex.map { case (s, c) =>
        if (grupo.contains((r + 1, c + 1))) Stone.Empty else s
      }
    }
  }

  private def dentroDoTabuleiro(coord: Coord2D, board: Board): Boolean = {
    val (r, c) = coord
    r >= 1 && c >= 1 && r <= board.size && c <= board.head.size
  }

  // T6 LP
  def checkVictory(playerCaptured: Int, computerCaptured: Int, captureGoal: Int): Option[String] = {
    if (playerCaptured >= captureGoal)
      Some("Jogador venceu")
    else if (computerCaptured >= captureGoal)
      Some("Computador venceu")
    else None
  }
  // Na função de captura passar o número de peças já captadas por ambos

  // função auxiliar para juntar: jogada, captura (atualizar valores) e verificar vitória
  def makeMove(state: GameState, coord: Coord2D, captureGoal: Int): (GameState, Option[String]) = {
    val (maybeNewBoard, newOpenCoords) = play(state.board, state.currentPlayer, coord, state.openCoords)

    maybeNewBoard match {
      case None =>
        // Jogada inválida → devolve o mesmo estado, sem vitória
        (state, None)

      case Some(boardAfterPlay) =>
        val (boardAfterCapture, updatedPlayerCaptured, updatedComputerCaptured) =
          captureGroupStones(boardAfterPlay, state.currentPlayer, state.playerCaptured, state.computerCaptured)

        val newState = GameState(
          board = boardAfterCapture,
          openCoords = newOpenCoords,
          currentPlayer = if (state.currentPlayer == Stone.Black) Stone.White else Stone.Black,
          playerCaptured = updatedPlayerCaptured,
          computerCaptured = updatedComputerCaptured
        )

        val result = checkVictory(updatedPlayerCaptured, updatedComputerCaptured, captureGoal)

        (newState, result)
    }
  }

  // T7 LP - medições calculadas no main
  def isTimeExceeded(startTime: Long, currentTime: Long, timeLimitMillis: Long): Boolean =
    (currentTime - startTime) > timeLimitMillis

  case class GameState(board: Board, openCoords: List[Coord2D], currentPlayer: Stone, playerCaptured: Int, computerCaptured: Int)
    type GameHistory = List[GameState]

  // undo funcional:
  def undo(history: GameHistory): (Option[GameState], GameHistory) = history match {
    case current :: previous :: rest => (Some(previous), previous :: rest)
    case current :: Nil => (Some(current), List(current)) // não há mais nada para desfazer
    case Nil => (None, Nil) // estado inválido
  }

}