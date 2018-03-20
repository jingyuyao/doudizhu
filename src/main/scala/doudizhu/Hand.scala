package doudizhu

case class Hand(cards: Set[Card]) {
  val won: Boolean = cards.isEmpty
  val plays: List[Play] = ???
}
