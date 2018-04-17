package doudizhu

/**
  * A more relaxed version of PlayingState that allows an agent to "play" for other agents. This
  * is typically used by a bot agent during simulation.
  */
class FakePlayingState(secretIdMap: Map[AgentSecret, AgentId],
                       hands: Map[AgentSecret, Cards],
                       startingHands: Map[AgentSecret, Cards],
                       creatorSecret: AgentSecret,
                       val landlord: AgentId,
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

  def isValid(agentId: AgentId, combo: Combo): Boolean = {
    val agentOwnsPlay =
      if (getAgentId(creatorSecret) == agentId) {
        combo.cards.set.subsetOf(hands(creatorSecret).set)
      }
      else {
        val hasEnoughCards = getNumCardsInHand(agentId) >= combo.cards.set.size
        val partOfOtherCards = combo.cards.set.subsetOf(otherCardsInPlay(creatorSecret).set)
        hasEnoughCards && partOfOtherCards
      }

    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => Play(agentId, combo).canBeat(lastPlay)
      case None => true
    }

    getWinner.isEmpty && agentOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given agent. It is up to the caller to ensure it is a valid play */
  def play(agentId: AgentId, combo: Combo): FakePlayingState = {
    if (!isValid(agentId, combo))
      throw new IllegalArgumentException("Invalid play")

    if (getAgentId(creatorSecret) == agentId) {
      val newCreatorHand = Cards(hands(creatorSecret).set.diff(combo.cards.set))
      val newHands = hands.updated(creatorSecret, newCreatorHand)

      new FakePlayingState(secretIdMap, newHands, startingHands, creatorSecret, landlord, plays :+ Play(agentId, combo))
    } else {
      // LOCAL SIDE EFFECTS!!!
      var remainingOtherCards = otherCardsInPlay(creatorSecret).set.diff(combo.cards.set).toList
      // Populates other agent hands with random cards.
      val newHands = hands.map({ case (secret, hand) =>
        if (secret == creatorSecret) {
          (secret, hand)
        } else if (agentId == getAgentId(secret)) {
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

      new FakePlayingState(secretIdMap, newHands, startingHands, creatorSecret, landlord, plays :+ Play(agentId, combo))
    }
  }
}
