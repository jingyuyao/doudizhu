package doudizhu

case class Agent(cards: Set[Card]) {
  val won: Boolean = cards.isEmpty
  /** All possible plays, very expensive operation. */
  lazy val allPlays: List[Play] = cards.subsets().flatMap(Play.make).toList

  /** Possible plays that can beat the `other` play, much cheaper than accessing `allPlays`. */
  def playsThatCanBeat(other: Play): List[Play] = {
    val rocketPlays =
      if (other.cards.size != 2 && Card.jokers.subsetOf(cards))
        List(Play.make(Card.jokers).get).filter(_.canBeat(other))
      else
        List()
    val bombPlays =
      if (other.cards.size != 4)
        cards.subsets(4).flatMap(Play.make).filter(_.canBeat(other))
      else
        List()
    val normalPlays = cards.subsets(other.cards.size).flatMap(Play.make).filter(_.canBeat(other))
    rocketPlays ++ bombPlays ++ normalPlays
  }
}
