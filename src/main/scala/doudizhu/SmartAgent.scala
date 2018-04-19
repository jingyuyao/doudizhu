package doudizhu

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.parallel.ParIterable
import scala.util.Random

/**
  * A smart agent that uses Expectimax, action pruning and heuristic features to find the best actions.
  */
class SmartAgent(agentId: AgentId,
                 agentSecret: AgentSecret,
                 maxDepth: Int = 2,
                 maxLayerSize: Int = 4) extends Agent(agentId, agentSecret) {
  private val DEBUG = true
  private val numGetSuccessor = new AtomicInteger()
  private val numSmartActions = new AtomicInteger()
  private val numLegalActions = new AtomicInteger()
  private val numAllActions = new AtomicInteger()

  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = eval(auctionState) > 0.5

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val startTime = System.nanoTime()
    if (DEBUG) {
      numGetSuccessor.set(0)
      numSmartActions.set(0)
      numLegalActions.set(0)
      numAllActions.set(0)
    }
    val fakePlayingState = playingState.toFake(agentSecret)
    val comboSuccessorPairs =
      getSmartActions(fakePlayingState, agentId).map(combo => (combo, fakePlayingState.play(agentId, combo)))
    val result =
      if (comboSuccessorPairs.nonEmpty) {
        val otherAgents = getOtherAgentsInOrder(fakePlayingState)
        val comboValues = comboSuccessorPairs.map({ case (combo, state) => (combo, minValue(state, 0, otherAgents)) })
        Some(comboValues.maxBy(_._2)._1)
      } else {
        None
      }
    if (DEBUG) {
      println(f"State expanded $numGetSuccessor, evaluated $numSmartActions")
      println(f"Action considered $numLegalActions, generated $numAllActions")
      println(f"Elapsed ${System.nanoTime() - startTime}%,dns")
    }
    result
  }

  private def maxValue(fakePlayingState: FakePlayingState, currentDepth: Int): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, agentId)
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val minValues = successorStates.map(state => minValue(state, currentDepth, otherAgents))
      if (minValues.isEmpty) Double.NegativeInfinity else minValues.max
    }

  private def minValue(fakePlayingState: FakePlayingState, currentDepth: Int, otherAgents: List[AgentId]): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, otherAgents.head)
      val maxValues =
        if (otherAgents.lengthCompare(1) == 0) {
          val nextDepth = currentDepth + 1
          successorStates.map(state => maxValue(state, nextDepth))
        }
        else {
          val remainingAgents = otherAgents.drop(1)
          successorStates.map(state => minValue(state, currentDepth, remainingAgents))
        }
      if (maxValues.isEmpty) Double.PositiveInfinity else maxValues.sum / maxValues.size
    }

  private def isTerminal(fakePlayingState: FakePlayingState, currentDepth: Int): Boolean =
    currentDepth == maxDepth || fakePlayingState.getWinner.nonEmpty

  private def getOtherAgentsInOrder(fakePlayingState: FakePlayingState): List[AgentId] = {
    val allAgentIds = fakePlayingState.getAllAgentIds
    val indexOfAgent = allAgentIds.indexOf(agentId)
    // Assumes round-robin style.
    allAgentIds.slice(indexOfAgent + 1, allAgentIds.size) ++ allAgentIds.slice(0, indexOfAgent)
  }

  private def getSuccessorStates(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[FakePlayingState] = {
    val result = getSmartActions(fakePlayingState, id).map(combo => fakePlayingState.play(id, combo))
    if (DEBUG) numGetSuccessor.incrementAndGet()
    result
  }

  private def getSmartActions(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[Combo] = {
    val legalActions = getLegalActions(fakePlayingState, id)
    // TODO: smart combo pruning
    val smartActions = legalActions.take(maxLayerSize)
    if (DEBUG) numSmartActions.addAndGet(smartActions.size)
    smartActions
  }

  private def getLegalActions(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[Combo] = {
    val legalActions = getAllActions(fakePlayingState, id).filter(combo => fakePlayingState.isValid(id, combo))
    if (DEBUG) numLegalActions.addAndGet(legalActions.size)
    legalActions
  }

  private def getAllActions(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[Combo] = {
    val availableCards =
      if (id == agentId)
        fakePlayingState.getHand(agentSecret)
      else
        fakePlayingState.otherCardsInPlay(agentSecret)
    val allActions = Combo.allFrom(availableCards)
    if (DEBUG) numAllActions.addAndGet(allActions.size)
    allActions.par
  }

  /** Evaluates the given auction state from this agent's perspective. */
  private def eval(auctionState: AuctionState): Double = Random.nextDouble()

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = Random.nextDouble()
}
