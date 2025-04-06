import Game.{Board, Coord2D, Stone, populateBoard, populateRows, MyRandom, randomMove}

object Main {
  def main(args: Array[String]): Unit = {
    val board: Board = populateBoard(3, 2)
    val coords = populateRows(3, 2)

    // teste T1
    val coordenadas: List[Coord2D] = List((0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (9, 9), (10, 10), (11, 11))
    val seed = MyRandom(System.currentTimeMillis())
    val (coordEsc, proxRand) = randomMove(coordenadas, seed)
    println(s"Movimento escolhido: $coordEsc")
  }
}
