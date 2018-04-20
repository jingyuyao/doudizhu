package doudizhu

case class Card(value: Int, suit: String) extends Ordered[Card] {
  require(1 <= value && value <= 15)

  /** Suit don't matter in DouDiZhu. */
  override def compare(that: Card): Int = this.value - that.value

  override def toString: String = value match {
    case 9 => f"J$suit"
    case 10 => f"Q$suit"
    case 11 => f"K$suit"
    case 12 => f"A$suit"
    case 13 => f"2$suit"
    case 14 => "B"
    case 15 => "R"
    // Value 1-8 corresponds to card 3-10
    case _ => f"${value + 2}$suit"
  }
}

case class Cards(set: Set[Card]) {
  lazy val sorted: List[Card] = set.toList.sorted
  lazy val getCombo: Option[Combo] = Combo.from(this)
  lazy val getAllCombo: Iterable[Combo] = Combo.allFrom(this)

  /** Get all the cards that partially contains the given string representation. */
  def apply(repr: String): Cards = Cards(set.filter(_.toString.contains(repr)))

  /** Get all the cards that partially contains any of the string representations. */
  def apply(reprs: String*): Cards =
    Cards(reprs.flatMap((repr) => set.filter(_.toString.contains(repr))).toSet)

  /** Select the first n elements. */
  def take(n: Int): Cards = Cards(set.take(n))

  override def toString: String = sorted.mkString(" ")
}

object Cards {
  //  val suits: Set[String] = Set("♠", "♥", "♦", "♣")
  // Suit don't really matter in DouDiZhu so we will just use simple to type characters.
  val suits: Set[String] = Set("+", "-", "*", "%")
  val suited: Cards = Cards(suits.flatMap((suit) => (1 to 13).map(Card(_, suit))))
  val jokers: Cards = Cards(Set(Card(14, ""), Card(15, "")))
  val all: Cards = Cards(suited.set ++ jokers.set)
}
