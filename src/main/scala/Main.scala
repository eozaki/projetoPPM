/*
Autores: Grupo LM8
Laís Gray
Luís Pinto
Erick Ozaki
*/

import Game.{Board, Coord2D, MyRandom, Stone, playRandomly, populateBoard, populateRows, randomMove}

object Main {
  def main(args: Array[String]): Unit = {
    val board: Board = populateBoard(3, 2)
    val coords = populateRows(3, 2)

    // teste T1
    val coordenadas: List[Coord2D] = Game.populateRows(5, 5)
    val seed: MyRandom = MyRandom(System.currentTimeMillis())
    val (coordEsc, proxRand) = randomMove(coordenadas, seed)
    println(s"Movimento escolhido: $coordEsc")

    // test T3
    val b = Game.populateBoard(5, 5)
    val s = Stone.White
    playRandomly(b, seed, s, coordenadas, Game.randomMove )

  }
}
