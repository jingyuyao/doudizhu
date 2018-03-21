package doudizhu

case class Card(value: Int, suit: String) extends Ordered[Card] {
  require(1 <= value && value <= 15)

  /** Suit don't matter in DouDiZhu. */
  override def compare(that: Card): Int = this.value - that.value

  override def toString: String = value match {
    case 9 => f" J$suit"
    case 10 => f" Q$suit"
    case 11 => f" K$suit"
    case 12 => f" A$suit"
    case 13 => f" 2$suit"
    case 14 => "BJK"
    case 15 => "RJK"
    // Value 1-8 corresponds to card 3-10
    case _ => f"${value + 2}%2d$suit"
  }
}

object Card {
  val suits: Set[String] = Set("♠", "♥", "♦", "♣")
  val suited: Set[Card] = suits.flatMap((suit) => (1 to 13).map(Card(_, suit)))
  val jokers: Set[Card] = Set(Card(14, ""), Card(15, ""))
  val all: Set[Card] = suited ++ jokers
  val sorted: List[Card] = all.toList.sorted

  /** Get all the cards that contains the given string representation. */
  def get(repr: String): Set[Card] = all.filter(_.toString.contains(repr))
}
