import Game.{Board, Coord2D}

object Main {
  def main(args: Array[String]): Unit = {
    val board: Board = List(List())
    val coords = populateRows(3, 2)
    Game(board, coords)
  }

  def populateRows(width: Int, height: Int): List[List[Coord2D]] = (width, height) match {
    case (0, _) => List()
    case (row, column) => populateColumns(row, column) ++ populateRows(row - 1, column)
  }

  def populateColumns(fixed: Int, changing: Int): List[List[Coord2D]] = (fixed, changing) match {
    case (_, 0) => List()
    case (row, column) => List(List((row, column))) ++ populateColumns(row, column - 1)
  }
}