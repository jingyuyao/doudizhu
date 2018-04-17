package doudizhu

import scala.util.Random

/**
  * A smart agent that uses Expectimax and various heuristics to find the best action.
  */
class SmartAgent(agentId: AgentId, agentSecret: AgentSecret, maxDepth: Int = 1) extends Agent(agentId, agentSecret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = eval(auctionState) > 0.5

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val fakePlayingState = playingState.toFake(agentSecret)
    val successors =
      getValidCombos(fakePlayingState, agentId)
        .map(combo => (combo, fakePlayingState.play(agentId, combo)))
    if (successors.isEmpty) {
      None
    } else {
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val comboValues = successors.map({ case (combo, state) => (combo, minValue(state, 0, otherAgents)) })
      Some(comboValues.maxBy(_._2)._1)
    }
  }

  private def maxValue(fakePlayingState: FakePlayingState, currentDepth: Int): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, agentId)
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val minValues = successorStates.map(state => minValue(state, currentDepth, otherAgents)) :+ Double.NegativeInfinity
      minValues.max
    }

  private def minValue(fakePlayingState: FakePlayingState, currentDepth: Int, otherAgents: List[AgentId]): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val maxValues =
        if (otherAgents.isEmpty) {
          getSuccessorStates(fakePlayingState, agentId)
            .map(state => maxValue(state, currentDepth + 1))
        } else {
          getSuccessorStates(fakePlayingState, otherAgents.head)
            .map(state => minValue(state, currentDepth, otherAgents.drop(1)))
        }
      if (maxValues.isEmpty)
        Double.PositiveInfinity
      else
        maxValues.sum / maxValues.size
    }

  private def isTerminal(fakePlayingState: FakePlayingState, currentDepth: Int): Boolean =
    fakePlayingState.getWinner.nonEmpty || currentDepth == maxDepth

  private def getSuccessorStates(fakePlayingState: FakePlayingState, agentId: AgentId): List[FakePlayingState] =
    getValidCombos(fakePlayingState, agentId).map(combo => fakePlayingState.play(agentId, combo))

  private def getValidCombos(fakePlayingState: FakePlayingState, agentId: AgentId): List[Combo] =
    if (agentId == agentId) {
      val hand = fakePlayingState.getHand(agentSecret)
      Combo.allFrom(hand).filter(combo => fakePlayingState.isValid(agentId, combo))
    } else {
      val otherCardsInPlay = fakePlayingState.otherCardsInPlay(agentSecret)
      Combo.allFrom(otherCardsInPlay).filter(combo => fakePlayingState.isValid(agentId, combo))
    }

  private def getOtherAgentsInOrder(fakePlayingState: FakePlayingState): List[AgentId] = {
    val allAgentIds = fakePlayingState.getAllAgentIds
    val indexOfAgent = allAgentIds.indexOf(agentId)
    // Assumes round-robin style.
    allAgentIds.slice(indexOfAgent + 1, allAgentIds.size) ++ allAgentIds.slice(0, indexOfAgent)
  }

  /** Evaluates the given auction state from this agent's perspective. */
  private def eval(auctionState: AuctionState): Double = Random.nextDouble()

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = Random.nextDouble()
}
