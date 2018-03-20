package doudizhu

case class Card private(value: Int, private val repr: String) extends Ordered[Card] {
  override def compare(that: Card): Int = this.value - that.value

  override def toString: String = this.repr
}

object Card {
  require(all.size == 54)
  require(sorted.lengthCompare(54) == 0)

  def all: Set[Card] =
    jokers ++
      genSuitedCards("♠") ++
      genSuitedCards("♥") ++
      genSuitedCards("♦") ++
      genSuitedCards("♣")

  def sorted: List[Card] = all.toList.sorted

  private def genSuitedCards(suit: String): Set[Card] = Set(
    Card(0, " 3" + suit),
    Card(1, " 4" + suit),
    Card(2, " 5" + suit),
    Card(3, " 6" + suit),
    Card(4, " 7" + suit),
    Card(5, " 8" + suit),
    Card(6, " 9" + suit),
    Card(7, "10" + suit),
    Card(8, " J" + suit),
    Card(9, " Q" + suit),
    Card(10, " K" + suit),
    Card(11, " A" + suit),
    Card(12, " 2" + suit),
  )

  private def jokers: Set[Card] = Set(
    Card(13, "BJK"),
    Card(14, "RJK"),
  )
}
