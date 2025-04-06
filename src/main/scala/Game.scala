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

  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = (coord, lstOpenCoords) match {
    case (c, l) if validCoord(c, l) => {
      val newBoard = playRow(c._1, c._2, player, board)
      val l = remainingCoords(lstOpenCoords, coord)
      displayBoard(newBoard)
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
    case(_, _, List()) => List()
    case(1, col, x :: xs) => playCol(col, x, player) :: xs
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
  private def displayBoard(board: Board): Unit = board match {
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
}