package doudizhu

import scala.util.Random

object DouDiZhu {
  def main(args: Array[String]): Unit = {
    for (card <- Card.sorted) {
      println(card)
    }
    val randomHand = Hand(Random.shuffle(Card.sorted).take(17).toSet ++ Card.jokers ++ Card.get("2"))
    val doubleTen = Play.make(Card.get("10").take(2)).get
    println(randomHand)
    println(doubleTen)
    println(randomHand.playsThatCanBeat(doubleTen))
  }
}
