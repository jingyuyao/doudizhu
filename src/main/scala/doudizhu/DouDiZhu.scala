package doudizhu

object DouDiZhu {
  def main(args: Array[String]): Unit = {
    for (card <- Card.sorted) {
      println(card)
    }
  }
}
