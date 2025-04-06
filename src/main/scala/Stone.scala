import Game._
import Game.Stone.Stone

case class Game(board: Board, coords: List[List[Coord2D]]) {
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

  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    lstOpenCoords match {
      case List() => throw new IllegalStateException("Posição inválida para a jogada")
      case x :: xs if x == coord => {
        Some()
      }
      case x :: xs => play(board, player, coord, xs)
    }
  }

}