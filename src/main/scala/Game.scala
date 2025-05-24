import Game._
import Game.Stone.Stone

import scala.annotation.tailrec

case class Game(board: Board, coords: List[Coord2D]) {
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]):(Option[Board], List[Coord2D]) =
    Game.play(board, player, coord, lstOpenCoords)

  def captureGroupStones(board: Board, player: Stone): (Board, Int) =
    Game.captureGroupStones(board, player)
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
//      displayBoard(newBoard) // n ter aña mm funçao
      (Some(newBoard), l)
    }
    case (c, l) if !validCoord(c, l) => (None, lstOpenCoords)
  }

  def surroundingCoords(coord: Coord2D, board: Board): List[Coord2D] = {
    val (row, col) = coord
    List((row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1))
      .filter { case (x, y) => x >= 1 && x <= board.length && y >= 1 && y <= board.head.length }
  }

  def samePlayerNeighbors(pos: Coord2D, board: Board): List[Coord2D] = {
    val player = stoneAt(pos._1, pos._2, board)
    surroundingCoords(pos, board).filter { case (row, col) => stoneAt(row, col, board) == player}
  }

  def samePlayerRegion(toVisit: List[Coord2D], group: Set[Coord2D], player: Stone, board: Board): List[Coord2D] = toVisit match {
    case List() => group.toList
    case pos :: remaining if group.contains(pos) => samePlayerRegion(remaining, group, player, board)
    case pos :: remaining if stoneAt(pos._1, pos._2, board) == player => samePlayerRegion(remaining ++ samePlayerNeighbors(pos, board), group.incl(pos), player, board)
    case _pos :: remaining => samePlayerRegion(remaining, group, player, board)
  }

  def hasFreePositionAround(position: Coord2D, board: Board): Boolean = {
    surroundingCoords(position, board).exists { case neighbor => stoneAt(neighbor._1, neighbor._2, board) == Stone.Empty }
  }

  def listPositions(board: Board, r: Int = 1): List[Coord2D] = {
    def listRowPositions(list: List[Stone], row: Int, col: Int = 1): List[Coord2D] = list match {
      case Nil => Nil
      case _ :: rest => (row, col) :: listRowPositions(rest, row, col + 1)
    }

    board match {
      case Nil => Nil
      case row :: rest => listRowPositions(row, r) ++ listPositions(rest, r + 1)
    }
  }

  def groupsAndLiberties(start: Coord2D, board: Board, player: Stone): (Set[Coord2D], Boolean) = {
    def liberties(lst: List[Coord2D], board: Board): Boolean = lst match {
      case List() => false
      case position :: _remaining if hasFreePositionAround(position, board) => true
      case _position :: remaining => liberties(remaining, board)
    }

    val g = samePlayerRegion(List(start), Set(), player, board).toSet
    val l = liberties(g.toList, board)

    (g, l)
  }

  def removeGroup(group: Set[Coord2D], board: Board): Board = {
    group.foldLeft(board)((acc, coord) => updateBoard(acc, coord, Stone.Empty))
  }

  def updateBoard(board: Board, coord: Coord2D, stone: Stone): Board = {
    val (row, col) = coord
    board.updated(row - 1, board(row - 1).updated(col - 1, stone))
  }

  def visitPositions(coords: List[Coord2D], board: Board, player: Stone, totalCaptured: Int = 0): (Board, Int) = coords match {
    case List() => (board, totalCaptured)
    case position :: remaining => groupsAndLiberties(position, board, player) match {
      case (group, false) => {
        val newB = removeGroup(group, board)
        visitPositions(remaining.filter { case pos => !group.contains(pos) }, newB, player, group.size + totalCaptured)
      }
      case (group, true) => {
        visitPositions(remaining.filter { case pos => !group.contains(pos) }, board, player, totalCaptured)
      }
    }
  }

  def captureGroupStones(board: Board, player: Stone): (Board, Int) = {
    val opponent = if (player == Stone.Black) Stone.White else Stone.Black

    val positionsList = listPositions(board)
    visitPositions(positionsList, board, opponent)
  }

  def stoneAt(pos: Coord2D, board: Board): Stone =
    stoneAt(pos._1, pos._2, board)

  def stoneAt(row: Int, col: Int, board: Board): Stone = {
    if (row < 1 || row > board.length || col < 1 || col > board.head.length) Stone.Empty
    else board(row - 1)(col - 1)
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

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = lstOpenCoords match {
    case List() => throw new IllegalArgumentException("Não há coordenadas livres disponíveis.") // caso a lista esteja vazia
    case list =>
      val (index, newRand) = rand.nextInt(list.size) // Gera um índice aleatório com base no tamanho da lista
      val chosenCoord = list(index) // Acede à coordenada da lista dada por index
      (chosenCoord, newRand) // Retorna a coord e nova seed para números aleatórios
  }

  def playSmart(board: Board, rand: MyRandom, player: Stone, lstOpenCoords: List[Coord2D]): (Coord2D, MyRandom) = {
    val opponentPositions = findAttackableStonesFor(opponentOf(player), board)
//    println(s"Posicoes adversarias a atacar: $opponentPositions")

    val (positionToAttack, newRand) = rand.nextInt(opponentPositions.length)
//    println(s"Posicao escolhida para atacar: $positionToAttack")

    val possiblePlays = surroundingCoords(opponentPositions(positionToAttack), board)
      .filter { case coord2D => lstOpenCoords.contains(coord2D) }
    val (chosenPlay, lastRand) = rand.nextInt(possiblePlays.length)

    (possiblePlays(chosenPlay), lastRand)
  }

  def chooseComputerMove(board: Board, lstOpenCoords: List[Coord2D], rand: MyRandom, player: Stone, settings: Settings): (Coord2D, MyRandom) = {
    val (smartOrDumb, newRand) = rand.nextInt(11) // 0-10 is the range of "intelligence" for the computer strategy
    smartOrDumb >= settings.difficulty match {
      case false => playSmart(board, newRand, player, lstOpenCoords)
      case true => randomMove(lstOpenCoords, newRand)
    }
  }

  def findAttackableStonesFor(player: Stone, board: Board): List[Coord2D] =
    listPositions(board).filter { case pos => stoneAt(pos, board) == player && hasFreePositionAround(pos, board) }

  def opponentOf(player: Stone) = if(player == Stone.Black) Stone.White else Stone.Black

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
        val (boardAfterCapture, opponentCaptured) =
          captureGroupStones(boardAfterPlay, state.currentPlayer)

        val newState = GameState(
          board = boardAfterCapture,
          openCoords = newOpenCoords,
          currentPlayer = if (state.currentPlayer == Stone.Black) Stone.White else Stone.Black,
          playerCaptured = if (state.currentPlayer != Stone.Black) 0 else opponentCaptured,
          computerCaptured = if(state.currentPlayer != Stone.Black) opponentCaptured else 0
        )

        val result = checkVictory(newState.playerCaptured, newState.computerCaptured, captureGoal)
        displayBoard(boardAfterCapture)

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