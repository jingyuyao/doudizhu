package doudizhu

class Card(private val value: Int, private val repr: String) extends Ordered[Card] {
  override def compare(that: Card): Int = this.value - that.value

  override def toString: String = this.repr
}

object Cards {

  def all: List[Card] =
    jokers ++
      genSuitedCards("♠") ++
      genSuitedCards("♥") ++
      genSuitedCards("♦") ++
      genSuitedCards("♣")

  def sorted: List[Card] = all.sorted

  private def genSuitedCards(suit: String): List[Card] = List(
    new Card(0, " 3" + suit),
    new Card(1, " 4" + suit),
    new Card(2, " 5" + suit),
    new Card(3, " 6" + suit),
    new Card(4, " 7" + suit),
    new Card(5, " 8" + suit),
    new Card(6, " 9" + suit),
    new Card(7, "10" + suit),
    new Card(8, " J" + suit),
    new Card(9, " Q" + suit),
    new Card(10, " K" + suit),
    new Card(11, " A" + suit),
    new Card(12, " 2" + suit),
  )

  private def jokers: List[Card] = List(
    new Card(13, "BJK"),
    new Card(14, "RJK"),
  )
}
