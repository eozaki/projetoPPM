import Game._
import Game.Stone.Stone

import scala.annotation.tailrec

// classe principal do jogo
// recebe: tabuleiro e lista de coordenadas 2D
case class Game(board: Board, coords: List[Coord2D]) { 
  // função de jogar
  // recebe: tabuleiro, jogador, coordenada e lista de coordenadas livres 
  // retorna: tabuleir e lista de coordenadas livres
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]):(Option[Board], List[Coord2D]) =
    Game.play(board, player, coord, lstOpenCoords)
}

// define o objeto singleton Game
object Game {
  // definição do objeto Stone com enumeração distinta
  object Stone extends Enumeration {
     // com alias Value para o tipo de stone
    type Stone = Value
    // enumeração dos valores possíveis
    val Black, White, Empty = Value
  }

  // definição de board como matriz de stones
  type Board = List[List[Stone]]
  // definição de coordenada como um tuplo de 2 int
  type Coord2D = (Int, Int) // (row, column)

  // T2
  // função de jogar no jogo
  // recebe: tabuleiro, jogador, coordenada e lista de coordenadas livres
  // retorna: tuplo com tabuleiro se jogada válida ou None se inválida e lista de coordenadas abertas após jogada
  def play(board: Board, player: Stone, coord: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = 
  // ao tuplo coordenada e lista de coordenadas disponíveis, procura correspondência
  (coord, lstOpenCoords) match {
    // à 1ª extração, se c e 1 são válidos 
    case (c, l) if validCoord(c, l) => {
      // newBoard é o resultado da função playRow
      val newBoard = playRow(c._1, c._2, player, board)
      // l é o resultado da função remainingCoords
      val l = remainingCoords(lstOpenCoords, coord)
      // chama a função displayBoard
      displayBoard(newBoard)
      // imprime a lista l (coordenadas disponíveis)
      println(l)
      // retorna o tuplo com newBoard, se a jogada foi válida e a lista de coordanedas disponíveis
      (Some(newBoard), l)
    }
    // se a extração são inválidos, devolve None e a lista de coordenadas
    case (c, l) if !validCoord(c, l) => (None, lstOpenCoords)
  }

  // retira uma coordenada da lista e devolve uma nova lista sem a coordenada
  // recebe: lista de coordenadas disponíveis e coordenada a retirar
  // retorna: lista de coordenadas disponíveis atualizada 
  private def remainingCoords(l: List[Coord2D], coord: Coord2D): List[Coord2D] = {
    l match {
      case List() => List()
      case (x, y) :: xs if x == coord._1 && y == coord._2 => xs
      case (x, y) :: xs if x != coord._1 || y != coord._2 => (x, y) :: remainingCoords(xs, coord)
    }
  }

  // joga uma pedra (branca ou preta) numa posição; a função navega pelas linhas até encontrar, delega a coluna à função playRow
  // recebe: dois Int, jogador (branco ou preto) e tabuleiro
  // retorna: novo tabuleiro com a posção ocupada
  private def playRow(row: Int, col: Int, player: Stone, board: Board): Board = (row, col, board) match {
    case (_, _, List()) => List()
    case (1, col, x :: xs) => playCol(col, x, player) :: xs
    case (n, col, x :: xs) => x :: playRow(n - 1, col, player, xs)
  }

  // invocada pela função playRow, encontra a coluna especificada da linha
  // recebe: Int que representa a coluna, lista de stones a atualizar e jogador(branco ou preto)
  // retorna: ndevolve lista de pedras com o valor col atualizado
  private def playCol(col: Int, list: List[Game.Stone.Stone], player: Game.Stone.Stone): List[Game.Stone.Stone] = (col, list) match {
    case (_, List()) => List()
    case (1, _ :: xs) => player :: xs
    case (n, x :: xs) => x :: playCol(n - 1, xs, player)
  }

  // verifica se uma coordenada é válida, ou seja, se existe na lista de jogadas disponíveis
  // recebe: coordenada, lista de coordenadas
  // retorna: true/false
  @tailrec
  private def validCoord(coord: Coord2D, coordList: List[Coord2D]): Boolean = coordList match {
    case List() => false
    case x :: _ if x == coord => true
    case _ :: xs => validCoord(coord, xs)
  }

  // Gera um tabuleiro de um tamanho indicado
  // recebe: int para a largura, int para a altura
  // retorna: um tabuleiro
  def populateBoard(width: Int, height: Int): Board = (width, height) match {
    case (0, _) => List()
    case (row, column) => populateBoardColumn(row, column) :: populateBoard(row - 1, column)
  }

  // gera uma lista com o número das colunas livres
  // recebe: int para o número da linha e int para o número de colunas
  // retorna: uma lista de posições vazias ??
  private def populateBoardColumn(fixed: Int, changing: Int): List[Game.Stone.Stone] = (fixed, changing) match {
    case (_, 0) => List()
    case (_row, column) => Stone.Empty :: populateBoardColumn(_row, column - 1)
  }

  // gera uma lista de todas as coordenadas válidas de um tabuleiro de dimensão x por Y 
  // recebe: int para a largura, int para a altura
  // retorna: lista de coordenadas
  def populateRows(width: Int, height: Int): List[Coord2D] = (width, height) match {
    case (0, _) => List()
    case (row, column) => populateColumns(row, column) ++ populateRows(row - 1, column)
  }

  // gera as coordenadas da coluna para uma linha especifica
  // recebe:int para a posição da linha e int para a posição da coluna
  // retorna: lista de coordenadas 2D
  private def populateColumns(fixed: Int, changing: Int): List[Coord2D] = (fixed, changing) match {
    case (_, 0) => List()
    case (row, column) => (row, column) :: populateColumns(row, column - 1)
  }

  // imprime recursivamente o tabuleiro
  // recebe: tabuleiro
  // retorna: unit
  @tailrec
  private def displayBoard(board: Board): Unit = board match {
    case Nil => ()
    case row :: tail =>
      displayRow(row)
      println()
      displayBoard(tail)
  }

  // imprime uma linha, convertendo uma pedra num caracter
  // recebe: lista de stones
  // retorna: unit
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

  // T1
  trait Random {
    def nextInt(x: Int): (Int, Random)
  }

  // implementação do trait random para gerar um número aleatório
  // recebe: seed gerador
  // retorna: int para o número gerado e uma instância Random
  case class MyRandom(seed: Long) extends Random {
    def nextInt(x: Int): (Int, MyRandom) = { // recebe um Int e devolve in Int e um random
      val newSeed = (seed * 0x11111111L + 0xBL) & 0xFFFFFFFFFFFFL // atualiza a seed com um novo valor
      val nextRandom = MyRandom(newSeed) // cria um novo MyRandom com a nova seed
      val n = (((newSeed >>> 16).toInt)) % x // gera um x aleatório [0, n-1]
      (if (n < 0) -n else n, nextRandom) // garante que o x está entre [0, n-1]
    }
  }

  // efetua um seleção random de uma lista de coordenadas disponíveis
  // recebe: lista de coordenadas disponíveis
  // retorna: coordenada e instância Random
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = lstOpenCoords match {
    case List() => throw new IllegalArgumentException("Não há coordenadas livres disponíveis.") // caso a lista esteja vazia
    case list =>
      val (index, newRand) = rand.nextInt(list.size) // Gera um índice aleatório com base no tamanho da lista
      val chosenCoord = list(index) // Acede à coordenada da lista dada por index
      (chosenCoord, newRand) // Retorna a coord e nova seed para números aleatórios
  }


  // T3
  // Executa uma jogada aleatória no jogo
  // recebe: tabuleiro, jogador, lista de coordenadas disponíveis e resultado de uma função que recebe uma lista de coordandas e MyRandom  devolve uma coordenada aleatória e uma instância Myrandom
  // retorna: tabuleiro, uma instância MyRandom e lista de coordanadas 2D disponíveis
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {

    val (coordAleat, novaSeed) = f(lstOpenCoords, r)
    val (updatedBoard, updatedOpenCoords: List[Coord2D]) = play(board, player, coordAleat, lstOpenCoords)

    updatedBoard match {
      case Some(updatedBoard) => (updatedBoard, novaSeed, updatedOpenCoords)
      case None => (board, novaSeed, lstOpenCoords)
    }
  }
}
