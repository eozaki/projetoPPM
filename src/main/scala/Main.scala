import Game.{Board, Coord2D, MyRandom, randomMove}

object Main {
  def main(args: Array[String]): Unit = {
    val board: Board = List(List())
    val coords = populateRows(3, 2)
    //Game(board, coords)

    // teste T1
    val coordenadas: List[Coord2D] = List((0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (9, 9), (10, 10), (11, 11))
    val seed = MyRandom(System.currentTimeMillis())
    val (coordEsc, proxRand) = randomMove(coordenadas, seed)
    println(s"Movimento escolhido: $coordEsc")


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