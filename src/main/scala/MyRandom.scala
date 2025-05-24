// T1
trait Random {
  def nextInt(x: Int): (Int, Random)
}

case class MyRandom(seed: Long) extends Random {
  def nextInt(x: Int): (Int, MyRandom) = { // recebe um Int e devolve in Int e um random
    val newSeed = (seed * 0x11111111L + 0xBL) & 0xFFFFFFFFFFFFL // atualiza a seed com um novo valor
    val nextRandom = MyRandom(newSeed) // cria um novo MyRandom com a nova seed
    val n = (((newSeed >>> 16).toInt)) % x // gera um x aleatório [0, n-1]
    (if (n < 0) -n else n, nextRandom) // garante que o x está entre [0, n-1]
  }
}