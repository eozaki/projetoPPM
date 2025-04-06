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

  // T1
  trait Random {
    def nextInt(x: Int): (Int, Random)
  }

  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, Random) = {                            // recebe um Int e devolve in Int e um random
      val newSeed = (seed * 0x11111111L + 0xBL) & 0xFFFFFFFFFFFFL    // atualiza a seed com um novo valor
      val nextRandom = MyRandom(newSeed)                              // cria um novo MyRandom com a nova seed
      val n = (((newSeed >>> 16).toInt)) % x                          // gera um x aleatório [0, n-1]
      (if (n < 0) -n else n, nextRandom)                              // garante que o x está entre [0, n-1]
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, Random) = {
    if (lstOpenCoords.isEmpty) throw new IllegalArgumentException("Não há coordenadas livres disponíveis.") // caso a lista esteja vazia
    val (index, newRand) = rand.nextInt(lstOpenCoords.size)           // Gera um índice aleatório com base no tamanho da lista
    val chosenCoord = lstOpenCoords(index)                            // Acede à coordenada da lista dada por index
    (chosenCoord, newRand)                                            // Retorna a coord e nova seed para números aleatórios
  }



}