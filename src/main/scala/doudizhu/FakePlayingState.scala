package doudizhu

/** A more relaxed version of PlayingState that allows an agent to "play" for other players. */
class FakePlayingState(hands: Map[PlayerSecret, Cards],
                       secretIdMap: Map[PlayerSecret, PlayerId],
                       startingHands: Map[PlayerSecret, Cards],
                       val landlord: PlayerId,
                       val plays: List[Play]) extends State(secretIdMap, hands) {
  // Each play can beat the last one.
  require(
    plays match {
      case Seq() => true
      case Seq(_) => true
      case _ => plays.sliding(2).forall({ case List(left, right) => right > left })
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
    val playerHasEnoughCards = numCardsInHand(id) >= combo.cards.set.size
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => Play(id, combo) > lastPlay
      case None => true
    }
    winner.isEmpty && playerHasEnoughCards && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(id: PlayerId, combo: Combo): FakePlayingState = {
    if (!isValid(id, combo))
      throw new IllegalArgumentException("Invalid play")

    new FakePlayingState(hands, secretIdMap, startingHands, landlord, plays :+ Play(id, combo))
  }
}
