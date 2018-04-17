package doudizhu

/**
  * A more relaxed version of PlayingState that allows an agent to "play" for other players. This
  * is typically used by a bot agent during simulation.
  */
class FakePlayingState(secretIdMap: Map[PlayerSecret, PlayerId],
                       hands: Map[PlayerSecret, Cards],
                       startingHands: Map[PlayerSecret, Cards],
                       creatorSecret: PlayerSecret,
                       val landlord: PlayerId,
                       val plays: List[Play]) extends State(secretIdMap, hands) {
  // Contains exactly one copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set.toList).fold(List())(_ ++ _)
    val cardsPlayed = plays.map(_.combo.cards.set.toList).fold(List())(_ ++ _)
    val cardsInState = cardsInHand ++ cardsPlayed
    Cards.all.set.size == cardsInState.size && Cards.all.set == cardsInState.toSet
  })
  // Each play can beat the last one.
  require(
    plays match {
      case Seq() => true
      case Seq(_) => true
      case _ => plays.sliding(2).forall({ case List(left, right) => right.canBeat(left) })
    }
  )
  // The only missing requirement compared to PlayingState is checking all plays can be derived
  // from the respective owner's hand.

  def isValid(id: PlayerId, combo: Combo): Boolean = {
    val playerOwnsPlay =
      if (getPlayerId(creatorSecret) == id) {
        combo.cards.set.subsetOf(hands(creatorSecret).set)
      }
      else {
        val hasEnoughCards = getNumCardsInHand(id) >= combo.cards.set.size
        val partOfOtherCards = combo.cards.set.subsetOf(otherCardsInPlay(creatorSecret).set)
        hasEnoughCards && partOfOtherCards
      }

    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => Play(id, combo).canBeat(lastPlay)
      case None => true
    }

    getWinner.isEmpty && playerOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(id: PlayerId, combo: Combo): FakePlayingState = {
    if (!isValid(id, combo))
      throw new IllegalArgumentException("Invalid play")

    if (getPlayerId(creatorSecret) == id) {
      val newCreatorHand = Cards(hands(creatorSecret).set.diff(combo.cards.set))
      val newHands = hands.updated(creatorSecret, newCreatorHand)

      new FakePlayingState(secretIdMap, newHands, startingHands, creatorSecret, landlord, plays :+ Play(id, combo))
    } else {
      // LOCAL SIDE EFFECTS!!!
      var remainingOtherCards = otherCardsInPlay(creatorSecret).set.diff(combo.cards.set).toList
      val newHands = hands.map({ case (secret, hand) =>
        if (secret == creatorSecret) {
          (secret, hand)
        } else if (id == getPlayerId(secret)) {
          val newHandSize = hand.set.size - combo.cards.set.size
          val newHand = remainingOtherCards.take(newHandSize)
          remainingOtherCards = remainingOtherCards.drop(newHandSize)
          (secret, Cards(newHand.toSet))
        } else {
          val newHand = remainingOtherCards.take(hand.set.size)
          remainingOtherCards = remainingOtherCards.drop(hand.set.size)
          (secret, Cards(newHand.toSet))
        }
      })
      assert(remainingOtherCards.isEmpty)

      new FakePlayingState(secretIdMap, newHands, startingHands, creatorSecret, landlord, plays :+ Play(id, combo))
    }
  }

  def otherCardsInPlay(secret: PlayerSecret): Cards =
    Cards(hands.filterKeys(_ != secret).values.flatMap(_.set).toSet)
}
