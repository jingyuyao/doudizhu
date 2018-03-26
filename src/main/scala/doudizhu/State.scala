package doudizhu

/** Never leak secrets. Prefer storing ID instead of secrets. */
trait State {
  protected val hands: Map[PlayerSecret, Cards]
  protected val secretIdMap: Map[PlayerSecret, PlayerId]

  def getHand(secret: PlayerSecret): Cards = hands(secret)

  def getPlayerId(secret: PlayerSecret): PlayerId = secretIdMap(secret)
}

case class AuctionState(protected val hands: Map[PlayerSecret, Cards],
                        protected val secretIdMap: Map[PlayerSecret, PlayerId],
                        private val chest: Cards) extends State {
  require(chest.set.size == 3)
  require(hands.values.map(_.set.size).forall(_ == 17))
  // Contains a copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ chest.set
  })

  def setLandlord(secret: PlayerSecret): PlayingState = {
    val landlordHand = Cards(getHand(secret).set.union(chest.set))
    val startingHands = hands.updated(secret, landlordHand)
    PlayingState(startingHands, secretIdMap, getPlayerId(secret), List(), startingHands)
  }
}

case class PlayingState(protected val hands: Map[PlayerSecret, Cards],
                        protected val secretIdMap: Map[PlayerSecret, PlayerId],
                        landlord: PlayerId,
                        plays: List[Play],
                        private val startingHands: Map[PlayerSecret, Cards]) extends State {
  // Contains a copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set).fold(Set())(_ ++ _)
    val cardsPlayed = plays.map(_.combo.cards.set).fold(Set())(_ ++ _)
    Cards.all.set == cardsInHand ++ cardsPlayed
  })
  // All plays can be derived from their respective owner's hands.
  require({
    hands.forall({
      case (secret, cardsInHand) =>
        val playedCards = plays.filter(_.id == getPlayerId(secret)).map(_.combo.cards.set).fold(Set())(_ ++ _)
        cardsInHand.set ++ playedCards == startingHands(secret).set
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

  def isValid(secret: PlayerSecret, combo: Combo): Boolean = {
    val playerOwnsPlay = combo.cards.set.subsetOf(hands(secret).set)
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => canBeat(Play(getPlayerId(secret), combo), lastPlay)
      case None => true
    }
    getWinner.isEmpty && playerOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(secret: PlayerSecret, combo: Combo): PlayingState = {
    if (!isValid(secret, combo))
      throw new IllegalArgumentException("Invalid play")

    val newPlayerHand = Cards(hands(secret).set.diff(combo.cards.set))
    val newHands = hands.updated(secret, newPlayerHand)
    PlayingState(newHands, secretIdMap, landlord, plays :+ Play(getPlayerId(secret), combo), startingHands)
  }

  /** Takes who made the play into consideration. */
  private def canBeat(newPlay: Play, lastPlay: Play) =
    lastPlay.id == newPlay.id || newPlay.combo.canBeat(lastPlay.combo)
}
