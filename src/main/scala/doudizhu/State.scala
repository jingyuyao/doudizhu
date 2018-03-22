package doudizhu

object State {
  val players: List[Player] = List(1, 2, 3)
}

trait State {
  val hands: Map[Player, Cards]
  val currentPlayer: Player
  val currentHand: Cards = hands(currentPlayer)
  val nextPlayer: Player = {
    val nextIndex = (State.players.indexOf(currentPlayer) + 1) % State.players.size
    State.players(nextIndex)
  }

  def pass(): State

  override def toString: String =
    f"Current player: $currentPlayer\nCurrent hand: $currentHand"
}

case class AuctionState(currentPlayer: Player,
                        hands: Map[Player, Cards],
                        originalPick: Player,
                        chest: Cards) extends State {
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ chest.set
  })

  def pass(): State = {
    if (nextPlayer == originalPick) {
      val oldHand = hands(originalPick)
      val newHand = Cards(oldHand.set.union(chest.set))
      val newHands = hands.updated(originalPick, newHand)
      PlayingState(originalPick, newHands, originalPick, List())
    }
    else {
      AuctionState(nextPlayer, hands, originalPick, chest)
    }
  }

  def acceptLandlord(): PlayingState = {
    val newHand = Cards(currentHand.set.union(chest.set))
    val newHands = hands.updated(currentPlayer, newHand)
    PlayingState(currentPlayer, newHands, currentPlayer, List())
  }

  override def toString: String = "Auctioning\n" + super.toString
}

case class PlayingState(currentPlayer: Player,
                        hands: Map[Player, Cards],
                        landlord: Player,
                        plays: List[(Player, Play)]) extends State {
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    val cardsPlayed = plays.map(_._2.cards.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ cardsPlayed
  })

  val winner: Option[Player] = hands.find(_._2.set.isEmpty).map(_._1)

  def pass(): State = PlayingState(nextPlayer, hands, landlord, plays)

  def play(play: Play): Option[PlayingState] = {
    // Current player must own this play
    if (!play.cards.set.subsetOf(currentHand.set))
      None

    if (plays.isEmpty || plays.last._1 == currentPlayer || play.canBeat(plays.last._2)) {
      val newHand = Cards(currentHand.set.diff(play.cards.set))
      val newHands = hands.updated(currentPlayer, newHand)
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
