package doudizhu

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.parallel.ParIterable
import scala.util.Random

/**
  * A smart agent that uses Expectimax, action pruning and heuristic features to find the best actions.
  */
class SmartAgent(agentId: AgentId,
                 agentSecret: AgentSecret,
                 maxDepth: Int = 2,
                 maxMaxLayerExpansions: Int = 4,
                 maxMinLayerExpansions: Int = 2) extends Agent(agentId, agentSecret) {
  private val debug = true
  private val numGetSuccessor = new AtomicInteger()
  private val numSuccessor = new AtomicInteger()

  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = eval(auctionState) > 0.5

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val startTime = System.nanoTime()
    if (debug) {
      numGetSuccessor.set(0)
      numSuccessor.set(0)
    }
    val fakePlayingState = playingState.toFake(agentSecret)
    val comboSuccessorPairs =
      getSmartCombos(fakePlayingState, agentId).map(combo => (combo, fakePlayingState.play(agentId, combo)))
    val result =
      if (comboSuccessorPairs.nonEmpty) {
        val otherAgents = getOtherAgentsInOrder(fakePlayingState)
        val comboValues = comboSuccessorPairs.map({ case (combo, state) => (combo, minValue(state, 0, otherAgents)) })
        Some(comboValues.maxBy(_._2)._1)
      } else {
        None
      }
    if (debug) {
      println(f"numGetSuccessor $numGetSuccessor")
      println(f"numSuccessor $numSuccessor")
      val elapsedSeconds = TimeUnit.SECONDS.convert(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
      println(f"elapsed time ${elapsedSeconds}s")
    }
    result
  }

  private def maxValue(fakePlayingState: FakePlayingState, currentDepth: Int): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      val result = eval(fakePlayingState)
      if (debug) println(f"max $currentDepth $result terminal")
      result
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, agentId)
      val otherAgents = getOtherAgentsInOrder(fakePlayingState)
      val minValues = successorStates.map(state => minValue(state, currentDepth, otherAgents)).toList :+ Double.NegativeInfinity
      val result = minValues.max
      if (debug) println(f"max $currentDepth $result")
      result
    }

  private def minValue(fakePlayingState: FakePlayingState, currentDepth: Int, otherAgents: List[AgentId]): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      val result = eval(fakePlayingState)
      if (debug) println(f"min $currentDepth $result terminal")
      result
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
      val result = if (maxValues.isEmpty) Double.PositiveInfinity else maxValues.sum / maxValues.size
      if (debug) println(f"min $currentDepth $result")
      result
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
    val result = getSmartCombos(fakePlayingState, id).map(combo => fakePlayingState.play(id, combo))
    if (debug) {
      numGetSuccessor.incrementAndGet()
      numSuccessor.addAndGet(result.size)
    }
    result
  }

  private def getSmartCombos(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[Combo] = {
    val forSelf = id == agentId
    val availableCards =
      if (forSelf)
        fakePlayingState.getHand(agentSecret)
      else
        fakePlayingState.otherCardsInPlay(agentSecret)
    val validCombos = Combo.allFrom(availableCards).filter(combo => fakePlayingState.isValid(id, combo))
    // TODO: smart combo pruning
    val sortedValidCombos = validCombos.sortWith(
      (l, r) => if (r.canBeat(l)) true else r.kind > l.kind)
    val maxNumCombos = if (forSelf) maxMaxLayerExpansions else maxMinLayerExpansions
    sortedValidCombos.take(maxNumCombos).par
  }

  /** Evaluates the given auction state from this agent's perspective. */
  private def eval(auctionState: AuctionState): Double = Random.nextDouble()

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = Random.nextDouble()
}
