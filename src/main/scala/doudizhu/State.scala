package doudizhu

object State {
  // Only supports three player variant of the game.
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
  require(chest.set.size == 3)
  require(hands.values.map(_.set.size).forall(_ == 17))
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ chest.set
  })

  def pass(): State = {
    if (nextPlayer == originalPick) {
      val landlordHand = Cards(hands(originalPick).set.union(chest.set))
      val startingHands = hands.updated(originalPick, landlordHand)
      PlayingState(originalPick, startingHands, originalPick, List(), startingHands)
    }
    else {
      AuctionState(nextPlayer, hands, originalPick, chest)
    }
  }

  def acceptLandlord(): PlayingState = {
    val landlordHand = Cards(currentHand.set.union(chest.set))
    val startingHands = hands.updated(currentPlayer, landlordHand)
    PlayingState(currentPlayer, startingHands, currentPlayer, List(), startingHands)
  }

  override def toString: String = "Auctioning\n" + super.toString
}

case class PlayingState(currentPlayer: Player,
                        hands: Map[Player, Cards],
                        landlord: Player,
                        plays: List[(Player, Play)],
                        startingHands: Map[Player, Cards]) extends State {
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    val cardsPlayed = plays.map(_._2.cards.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ cardsPlayed
  })
  require({
    hands.forall({
      case (player, cardsInHand) =>
        val playedCards = plays.filter(_._1 == player).map(_._2.cards.set).fold(Set())(_ ++ _)
        cardsInHand.set ++ playedCards == startingHands(player).set
    })
  })

  val winner: Option[Player] = hands.find(_._2.set.isEmpty).map(_._1)

  def pass(): State = PlayingState(nextPlayer, hands, landlord, plays, startingHands)

  def play(play: Play): Option[PlayingState] = {
    // Current player must own this play
    if (!play.cards.set.subsetOf(currentHand.set))
      None

    if (plays.isEmpty || plays.last._1 == currentPlayer || play.canBeat(plays.last._2)) {
      val newHand = Cards(currentHand.set.diff(play.cards.set))
      val newHands = hands.updated(currentPlayer, newHand)
      Some(PlayingState(nextPlayer, newHands, landlord, plays :+ (currentPlayer, play), startingHands))
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
