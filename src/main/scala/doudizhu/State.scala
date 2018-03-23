package doudizhu

/** Never leak PlayerKeys. */
trait State {
  protected val hands: Map[PlayerKey, Cards]
  protected val playerIds: Map[PlayerKey, PlayerId]

  def getHand(playerKey: PlayerKey): Cards = hands(playerKey)

  def getPlayerId(playerKey: PlayerKey): PlayerId = playerIds(playerKey)
}

case class AuctionState(protected val hands: Map[PlayerKey, Cards],
                        protected val playerIds: Map[PlayerKey, PlayerId],
                        private val chest: Cards) extends State {
  require(chest.set.size == 3)
  require(hands.values.map(_.set.size).forall(_ == 17))
  // Contains a copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ chest.set
  })

  def setLandlord(playerKey: PlayerKey): PlayingState = {
    val landlordHand = Cards(getHand(playerKey).set.union(chest.set))
    val startingHands = hands.updated(playerKey, landlordHand)
    PlayingState(startingHands, playerIds, playerKey, List(), startingHands)
  }
}

case class PlayingState(protected val hands: Map[PlayerKey, Cards],
                        protected val playerIds: Map[PlayerKey, PlayerId],
                        private val landlord: PlayerKey,
                        private val plays: List[(PlayerKey, Play)],
                        private val startingHands: Map[PlayerKey, Cards]) extends State {
  // Contains a copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    val cardsPlayed = plays.map(_._2.cards.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ cardsPlayed
  })
  // All plays can be derived from their respective owner's hands.
  require({
    hands.forall({
      case (player, cardsInHand) =>
        val playedCards = plays.filter(_._1 == player).map(_._2.cards.set).fold(Set())(_ ++ _)
        cardsInHand.set ++ playedCards == startingHands(player).set
    })
  })
  // Each play can beat the last one.
  require(
    plays match {
      case Seq() => true
      case Seq(_) => true
      case _ => plays.sliding(2).forall({ case List(left, right) => canBeat(right, left) })
    }
  )

  def getWinner: Option[PlayerId] = hands.find(_._2.set.isEmpty).map(_._1).map(getPlayerId)

  def getLandlord: PlayerId = getPlayerId(landlord)

  def getPlays: List[(PlayerId, Play)] = plays.map((play) => (getPlayerId(play._1), play._2))

  def isValid(playerKey: PlayerKey, play: Play): Boolean = {
    val playerOwnsPlay = play.cards.set.subsetOf(hands(playerKey).set)
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => canBeat((playerKey, play), lastPlay)
      case None => true
    }
    getWinner.isEmpty && playerOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(playerKey: PlayerKey, play: Play): PlayingState = {
    if (!isValid(playerKey, play))
      throw new IllegalArgumentException("Invalid play")

    val newPlayerHand = Cards(hands(playerKey).set.diff(play.cards.set))
    val newHands = hands.updated(playerKey, newPlayerHand)
    PlayingState(newHands, playerIds, landlord, plays :+ (playerKey, play), startingHands)
  }

  /** Takes who made the play into consideration. */
  private def canBeat(newPlay: (PlayerKey, Play), lastPlay: (PlayerKey, Play)) =
    lastPlay._1 == newPlay._1 || newPlay._2.canBeat(lastPlay._2)
}
