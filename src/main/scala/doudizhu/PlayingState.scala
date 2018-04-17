package doudizhu

class PlayingState(secretIdMap: Map[AgentSecret, AgentId],
                   hands: Map[AgentSecret, Cards],
                   startingHands: Map[AgentSecret, Cards],
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
  // All plays can be derived from their respective owner's hands.
  require({
    hands.forall({
      case (secret, cardsInHand) =>
        val playedCards = plays.filter(_.agentId == getAgentId(secret)).map(_.combo.cards.set).fold(Set())(_ ++ _)
        cardsInHand.set ++ playedCards == startingHands(secret).set
    })
  })

  def isValid(agentSecret: AgentSecret, combo: Combo): Boolean = {
    val playerOwnsPlay = combo.cards.set.subsetOf(hands(agentSecret).set)
    val beatLastPlay = plays.lastOption match {
      case Some(lastPlay) => Play(getAgentId(agentSecret), combo).canBeat(lastPlay)
      case None => true
    }
    getWinner.isEmpty && playerOwnsPlay && beatLastPlay
  }

  /** Make a new play from the given player. It is up to the caller to ensure it is a valid play */
  def play(agentSecret: AgentSecret, combo: Combo): PlayingState = {
    if (!isValid(agentSecret, combo))
      throw new IllegalArgumentException("Invalid play")

    val newPlayerHand = Cards(hands(agentSecret).set.diff(combo.cards.set))
    val newHands = hands.updated(agentSecret, newPlayerHand)
    new PlayingState(secretIdMap, newHands, startingHands, landlord, plays :+ Play(getAgentId(agentSecret), combo))
  }

  def toFake(secret: AgentSecret): FakePlayingState =
    new FakePlayingState(secretIdMap, hands, startingHands, secret, landlord, plays)
}
