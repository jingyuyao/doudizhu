package doudizhu

trait State

case class AuctionState(hands: Map[Player, Hand], chest: Set[Card]) extends State {
  require({
    val cardsInHand = hands.map(_._2.cards).reduce(_ ++ _)
    Card.all == cardsInHand ++ chest
  })

  val currentPlayer: Player = ???

  def ascension(player: Player): PlayingState = ???
}

case class PlayingState(hands: Map[Player, Hand], plays: List[(Player, Play)]) extends State {
  require({
    val cardsInHand = hands.map(_._2.cards).reduce(_ ++ _)
    val cardsPlayed = plays.map(_._2.cards).reduce(_ ++ _)
    Card.all == cardsInHand ++ cardsPlayed
  })

  val currentPlayer: Player = ???

  def play(play: Option[Play]): PlayingState = ???
}
