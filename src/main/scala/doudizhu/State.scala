package doudizhu

object State {
  val players: List[Player] = List(1, 2, 3)
}

trait State {
  val hands: Map[Player, Hand]
  val currentPlayer: Player
  val currentHand: Hand = hands(currentPlayer)
  val nextPlayer: Player = {
    val nextIndex = (State.players.indexOf(currentPlayer) + 1) % State.players.size
    State.players(nextIndex)
  }

  def pass(): State

  override def toString: String =
    f"Current player: $currentPlayer\nCurrent hand: ${Card.sortedString(currentHand)}"
}

case class AuctionState(currentPlayer: Player,
                        hands: Map[Player, Hand],
                        originalPick: Player,
                        chest: Set[Card]) extends State {
  require({
    val cardsInHand = hands.values.fold(Set())(_ ++ _)
    Card.all == cardsInHand ++ chest
  })

  def pass(): State = {
    if (nextPlayer == originalPick) {
      val newHands = hands.updated(nextPlayer, hands(nextPlayer).union(chest))
      PlayingState(nextPlayer, newHands, nextPlayer, List())
    }
    else {
      AuctionState(nextPlayer, hands, originalPick, chest)
    }
  }

  def acceptLandlord(): PlayingState = {
    val newHands = hands.updated(currentPlayer, currentHand.union(chest))
    PlayingState(currentPlayer, newHands, currentPlayer, List())
  }

  override def toString: String = "Auctioning\n" + super.toString
}

case class PlayingState(currentPlayer: Player,
                        hands: Map[Player, Hand],
                        landlord: Player,
                        plays: List[(Player, Play)]) extends State {
  require({
    val cardsInHand = hands.values.fold(Set())(_ ++ _)
    val cardsPlayed = plays.map(_._2.cards).fold(Set())(_ ++ _)
    Card.all == cardsInHand ++ cardsPlayed
  })

  val winner: Option[Player] = hands.find(_._2.isEmpty).map(_._1)

  def pass(): State = PlayingState(nextPlayer, hands, landlord, plays)

  def play(play: Play): Option[PlayingState] = {
    // Current player must own this play
    if (!play.cards.subsetOf(currentHand))
      None

    if (plays.isEmpty || plays.last._1 == currentPlayer || play.canBeat(plays.last._2)) {
      val newHands = hands.updated(currentPlayer, currentHand.diff(play.cards))
      Some(PlayingState(nextPlayer, newHands, landlord, plays :+ (currentPlayer, play)))
    }
    else {
      None
    }
  }

  override def toString: String = {
    val lastPlay =
      if (plays.isEmpty) {
        "New game\n"
      }
      else {
        f"Last play: player ${plays.last._1} played ${plays.last._2}\n"
      }
    lastPlay + super.toString
  }
}
