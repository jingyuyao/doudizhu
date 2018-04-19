package doudizhu

import scala.util.Random

/**
  * A smart agent that uses Expectimax and various heuristics to find the best action.
  */
class SmartAgent(agentId: AgentId, agentSecret: AgentSecret, maxDepth: Int = 1) extends Agent(agentId, agentSecret) {
  private val debug = true

  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = eval(auctionState) > 0.5

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val fakePlayingState = playingState.toFake(agentSecret)
    val comboSuccessorPairs =
      getSmartCombos(fakePlayingState, agentId).map(combo => (combo, fakePlayingState.play(agentId, combo)))
    if (comboSuccessorPairs.nonEmpty) {
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val comboValues = comboSuccessorPairs.map({ case (combo, state) => (combo, minValue(state, 0, otherAgents)) })
      Some(comboValues.maxBy(_._2)._1)
    } else {
      None
    }
  }

  private def maxValue(fakePlayingState: FakePlayingState, currentDepth: Int): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      val value = eval(fakePlayingState)
      if (debug) println(f"max $currentDepth $value terminal")
      value
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, agentId)
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val minValues = successorStates.map(state => minValue(state, currentDepth, otherAgents)) :+ Double.NegativeInfinity
      val value = minValues.max
      if (debug) println(f"max $currentDepth $value")
      value
    }

  private def minValue(fakePlayingState: FakePlayingState, currentDepth: Int, otherAgents: List[AgentId]): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      val value = eval(fakePlayingState)
      if (debug) println(f"min $currentDepth $value terminal")
      value
    } else {
      println(f"min $currentDepth")
      val successorStates = getSuccessorStates(fakePlayingState, otherAgents.head)
      val maxValues =
        if (otherAgents.lengthCompare(1) == 0)
          successorStates.map(state => maxValue(state, currentDepth + 1))
        else
          successorStates.map(state => minValue(state, currentDepth, otherAgents.drop(1)))
      val value = if (maxValues.isEmpty) Double.PositiveInfinity else maxValues.sum / maxValues.size
      if (debug) println(f"min $currentDepth $value")
      value
    }

  private def isTerminal(fakePlayingState: FakePlayingState, currentDepth: Int): Boolean =
    currentDepth == maxDepth || fakePlayingState.getWinner.nonEmpty

  private def getOtherAgentsInOrder(fakePlayingState: FakePlayingState): List[AgentId] = {
    val allAgentIds = fakePlayingState.getAllAgentIds
    val indexOfAgent = allAgentIds.indexOf(agentId)
    // Assumes round-robin style.
    allAgentIds.slice(indexOfAgent + 1, allAgentIds.size) ++ allAgentIds.slice(0, indexOfAgent)
  }

  private def getSuccessorStates(fakePlayingState: FakePlayingState, id: AgentId): List[FakePlayingState] =
    getSmartCombos(fakePlayingState, id).map(combo => fakePlayingState.play(id, combo))

  private def getSmartCombos(fakePlayingState: FakePlayingState, id: AgentId): List[Combo] = {
    val availableCards =
      if (id == agentId)
        fakePlayingState.getHand(agentSecret)
      else
        fakePlayingState.otherCardsInPlay(agentSecret)
    val validCombos = Combo.allFrom(availableCards).filter(combo => fakePlayingState.isValid(id, combo))
    // TODO: smart combo pruning
    validCombos.take(4)
  }

  /** Evaluates the given auction state from this agent's perspective. */
  private def eval(auctionState: AuctionState): Double = Random.nextDouble()

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = Random.nextDouble()
}
