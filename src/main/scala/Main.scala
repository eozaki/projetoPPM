/*
Autores: Grupo LM8
Laís Gray
Luís Pinto
Erick Ozaki
*/

import Game.{Board, Coord2D, Stone, populateBoard, populateRows, randomMove}
import Game._
import Game.Stone.Stone
import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    new Tui().TUI.start()
  }
}
