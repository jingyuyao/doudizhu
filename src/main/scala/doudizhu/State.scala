package doudizhu

/** Never leak secrets. Prefer storing ID instead of secrets. */
trait State {
  protected val hands: Map[PlayerSecret, Cards]
  protected val secretIdMap: Map[PlayerSecret, PlayerId]

  lazy val numCardsInHand: Map[PlayerId, Int] =
    hands.map({ case (secret, cards) => (getPlayerId(secret), cards.set.size) })

  lazy val winner: Option[PlayerId] = numCardsInHand.find(_._2 == 0).map(_._1)

  def getHand(secret: PlayerSecret): Cards = hands(secret)

  def getPlayerId(secret: PlayerSecret): PlayerId = secretIdMap(secret)
}

trait Playing extends State {
  val landlord: PlayerId
  val plays: List[Play]

  def otherCardsInPlay(secret: PlayerSecret): Cards = {
    val cardsInPlayerHand = hands(secret).set
    val cardsPlayed = plays.map(_.combo.cards.set).fold(Set())(_ ++ _)
    Cards(Cards.all.set.diff(cardsInPlayerHand).diff(cardsPlayed))
  }

  protected def canBeat(newPlay: Play, lastPlay: Play): Boolean =
    lastPlay.id == newPlay.id || newPlay.combo.canBeat(lastPlay.combo)
}

case class AuctionState(protected val hands: Map[PlayerSecret, Cards],
                        protected val secretIdMap: Map[PlayerSecret, PlayerId],
                        private val chest: Cards) extends State {
  require(chest.set.size == 3)
  require(hands.values.map(_.set.size).forall(_ == 17))
  // Contains exactly one copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set.toList).fold(List())(_ ++ _)
    val cardsInState = cardsInHand ++ chest.set.toList
    Cards.all.set.size == cardsInState.size && Cards.all.set == cardsInState.toSet
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
                        private val startingHands: Map[PlayerSecret, Cards]) extends Playing {
  // Contains exactly one copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set.toList).fold(List())(_ ++ _)
    val cardsPlayed = plays.map(_.combo.cards.set.toList).fold(List())(_ ++ _)
    val cardsInState = cardsInHand ++ cardsPlayed
    Cards.all.set.size == cardsInState.size && Cards.all.set == cardsInState.toSet
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

  def isValid(secret: PlayerSecret, combo: Combo): Boolean = {
    val playerOwnsPlay = combo.cards.set.subsetOf(hands(secret).set)
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => canBeat(Play(getPlayerId(secret), combo), lastPlay)
      case None => true
    }
    winner.isEmpty && playerOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(secret: PlayerSecret, combo: Combo): PlayingState = {
    if (!isValid(secret, combo))
      throw new IllegalArgumentException("Invalid play")

    val newPlayerHand = Cards(hands(secret).set.diff(combo.cards.set))
    val newHands = hands.updated(secret, newPlayerHand)
    PlayingState(newHands, secretIdMap, landlord, plays :+ Play(getPlayerId(secret), combo), startingHands)
  }

  def toFake: FakePlayingState = FakePlayingState(hands, secretIdMap, landlord, plays, startingHands)
}

/** A more relaxed version of PlayingState that allows an agent to "play" for other players. */
case class FakePlayingState(protected val hands: Map[PlayerSecret, Cards],
                            protected val secretIdMap: Map[PlayerSecret, PlayerId],
                            landlord: PlayerId,
                            plays: List[Play],
                            private val startingHands: Map[PlayerSecret, Cards]) extends Playing {
  // Each play can beat the last one.
  require(
    plays match {
      case Seq() => true
      case Seq(_) => true
      case _ => plays.sliding(2).forall({ case List(left, right) => canBeat(right, left) })
    }
  )

  override lazy val numCardsInHand: Map[PlayerId, Int] = {
    val numCardsPlayedByEachPlayer =
      plays.groupBy(_.id).mapValues(l => l.foldLeft(0)((s, p) => s + p.combo.cards.set.size))

    startingHands.map({
      case (secret, cards) =>
        (getPlayerId(secret), cards.set.size - numCardsPlayedByEachPlayer(getPlayerId(secret)))
    })
  }

  override def getHand(secret: PlayerSecret): Cards =
    throw new IllegalStateException("You should not rely on hand data in a fake state")

  def isValid(id: PlayerId, combo: Combo): Boolean = {
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => canBeat(Play(id, combo), lastPlay)
      case None => true
    }
    winner.isEmpty && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(id: PlayerId, combo: Combo): PlayingState = {
    if (!isValid(id, combo))
      throw new IllegalArgumentException("Invalid play")

    PlayingState(hands, secretIdMap, landlord, plays :+ Play(id, combo), startingHands)
  }
}
