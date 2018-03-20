package doudizhu

object DouDiZhu {
  def main(args: Array[String]): Unit = {
    for (card <- Cards.sorted) {
      println(card)
    }
  }
}
