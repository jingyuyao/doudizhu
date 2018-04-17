package doudizhu

import scala.util.Random

/**
  * A smart agent that uses Expectimax and various heuristics to find the best action.
  */
class SmartAgent(id: PlayerId, secret: PlayerSecret, maxDepth: Int = 1) extends Agent(id, secret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = eval(auctionState) > 0.5

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val fakePlayingState = playingState.toFake(secret)
    val successors =
      getValidCombos(fakePlayingState, id)
        .map(combo => (combo, fakePlayingState.play(id, combo)))
    if (successors.isEmpty) {
      None
    } else {
      val otherPlayers = getOtherPlayersInOrder(fakePlayingState)
      val comboValues = successors.map({ case (combo, state) => (combo, minValue(state, 0, otherPlayers)) })
      Some(comboValues.maxBy(_._2)._1)
    }
  }

  private def maxValue(fakePlayingState: FakePlayingState, currentDepth: Int): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, id)
      val otherPlayers = getOtherPlayersInOrder(fakePlayingState)
      val minValues = successorStates.map(state => minValue(state, currentDepth, otherPlayers)) :+ Double.NegativeInfinity
      minValues.max
    }

  private def minValue(fakePlayingState: FakePlayingState, currentDepth: Int, otherPlayers: List[PlayerId]): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val maxValues =
        if (otherPlayers.isEmpty) {
          getSuccessorStates(fakePlayingState, id)
            .map(state => maxValue(state, currentDepth + 1))
        } else {
          getSuccessorStates(fakePlayingState, otherPlayers.head)
            .map(state => minValue(state, currentDepth, otherPlayers.drop(1)))
        }
      if (maxValues.isEmpty)
        Double.PositiveInfinity
      else
        maxValues.sum / maxValues.size
    }

  private def isTerminal(fakePlayingState: FakePlayingState, currentDepth: Int): Boolean =
    fakePlayingState.getWinner.nonEmpty || currentDepth == maxDepth

  private def getSuccessorStates(fakePlayingState: FakePlayingState, playerId: PlayerId): List[FakePlayingState] =
    getValidCombos(fakePlayingState, playerId).map(combo => fakePlayingState.play(playerId, combo))

  private def getValidCombos(fakePlayingState: FakePlayingState, playerId: PlayerId): List[Combo] =
    if (playerId == id) {
      val hand = fakePlayingState.getHand(secret)
      Combo.allFrom(hand).filter(combo => fakePlayingState.isValid(playerId, combo))
    } else {
      val otherCardsInPlay = fakePlayingState.otherCardsInPlay(secret)
      Combo.allFrom(otherCardsInPlay).filter(combo => fakePlayingState.isValid(playerId, combo))
    }

  private def getOtherPlayersInOrder(fakePlayingState: FakePlayingState): List[PlayerId] = {
    val allPlayerIds = fakePlayingState.getAllPlayerIds
    val indexOfAgent = allPlayerIds.indexOf(id)
    // Assumes round-robin style.
    allPlayerIds.slice(indexOfAgent + 1, allPlayerIds.size) ++ allPlayerIds.slice(0, indexOfAgent)
  }

  /** Evaluates the given auction state from this agent's perspective. */
  private def eval(auctionState: AuctionState): Double = Random.nextDouble()

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = Random.nextDouble()
}
