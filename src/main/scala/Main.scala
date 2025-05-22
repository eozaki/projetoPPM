/*
Autores: Grupo LM8
Laís Gray
Luís Pinto
Erick Ozaki
*/

import Game.{Board, Coord2D, MyRandom, Stone, playRandomly, populateBoard, populateRows, randomMove}

object Main {
  def main(args: Array[String]): Unit = {
//    val board: Board = populateBoard(3, 2)
//    val coords = populateRows(3, 2)

    // teste T1
    val coordenadas: List[Coord2D] = Game.populateRows(5, 5)
    val seed: MyRandom = MyRandom(System.currentTimeMillis())
    val (coordEsc, proxRand) = randomMove(coordenadas, seed)
    println(s"Movimento escolhido: $coordEsc")

    // test T3
    val board = Game.populateBoard(5, 5)
    val w = Stone.White
    val b = Stone.Black
//    playRandomly(board, seed, s, coordenadas, Game.randomMove )

    val (b1, c1) = Game.play(board, w, (1, 1), coordenadas)
    val (b2, c2) = Game.play(b1.get, b, (1, 2), c1)
    val (b3, c3) = Game.play(b2.get, b, (2, 1), c2)

    val (b4, i) = Game.captureGroupStones(b3.get, b)

    Game.displayBoard(b4)
    println(s"$i peças capturadas")
  }
}
