package doudizhu

import java.util.concurrent.atomic.AtomicInteger

import doudizhu.ComboKind._

import scala.collection.parallel.ParIterable

/**
  * A smart agent that uses Expectimax, action pruning and heuristic features to find the best actions.
  */
class SmartAgent(agentId: AgentId,
                 agentSecret: AgentSecret,
                 maxDepth: Int = 2,
                 maxExpectiLayerExpansion: Int = 2) extends Agent(agentId, agentSecret) {
  private val DEBUG = true
  private val VERBOSE = false
  private val numGetSuccessor = new AtomicInteger()
  private val numEval = new AtomicInteger()
  private val numSmartActions = new AtomicInteger()
  private val numLegalActions = new AtomicInteger()
  private val numAllActions = new AtomicInteger()

  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = {
    val hand = auctionState.getHand(agentSecret)
    val handCombos = Combo.allFrom(hand)
    val handComboValues = handCombos.map(smartComboValue)
    val averageHandComboValue = handComboValues.sum.toDouble / handComboValues.size

    if (DEBUG) println(f"    auction combo avg $averageHandComboValue")

    averageHandComboValue > 20
  }

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val startTime = System.nanoTime()
    if (DEBUG) {
      numGetSuccessor.set(0)
      numEval.set(0)
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
        val maxComboValue = comboValues.maxBy(_._2)
        val currentStateValue = eval(fakePlayingState)
        if (DEBUG) println(f"    current state $currentStateValue, max combo $maxComboValue")
        if (hasInitiative(fakePlayingState) || maxComboValue._2 >= currentStateValue)
          Some(maxComboValue._1)
        else
          None
      } else {
        None
      }
    if (DEBUG) {
      println(f"    getSuccessor $numGetSuccessor, eval $numEval, smart $numSmartActions, legal $numLegalActions, all $numAllActions")
      println(f"    elapsed ${System.nanoTime() - startTime}%,dns")
      println(f"    hand ${playingState.getHand(agentSecret)}")
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
      minValues.max
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
      maxValues.sum / maxValues.size
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
    // Can always just pass
    result ++ Seq(fakePlayingState)
  }

  private def getSmartActions(fakePlayingState: FakePlayingState, id: AgentId): ParIterable[Combo] = {
    val legalActions = getLegalActions(fakePlayingState, id)
    val smartActions =
      if (agentId == id)
        legalActions
      else
        legalActions.toList.sortBy(smartComboValue).take(maxExpectiLayerExpansion).par

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
        fakePlayingState.getUnseenCards(agentSecret)
    val allActions = Combo.allFrom(availableCards)
    if (DEBUG) numAllActions.addAndGet(allActions.size)
    allActions.par
  }

  /** Evaluates the given fake playing state from this agent's perspective. */
  private def eval(fakePlayingState: FakePlayingState): Double = {
    if (DEBUG) numEval.incrementAndGet()
    fakePlayingState.getWinner match {
      case Some(winner) =>
        if (winner == agentId || (agentId != fakePlayingState.landlord && winner != fakePlayingState.landlord))
          Double.PositiveInfinity
        else
          Double.NegativeInfinity
      case None =>
        val hand = fakePlayingState.getHand(agentSecret)
        val handCombos = Combo.allFrom(hand)
        val handComboValuesSorted = handCombos.map(smartComboValue).toList.sorted

        val numCardsInHandFeature = 500.0 / hand.set.size
        val medianHandComboValueFeature =
          if (handComboValuesSorted.size % 2 == 1) {
            handComboValuesSorted(handComboValuesSorted.size / 2).toDouble
          }
          else {
            val (up, down) = handComboValuesSorted.splitAt(handComboValuesSorted.size / 2)
            (up.last + down.head) / 2.0
          }

        val reward = numCardsInHandFeature + medianHandComboValueFeature

        if (DEBUG && VERBOSE)
          println(f"    eval $numCardsInHandFeature%.1f $medianHandComboValueFeature%.1f $reward%.1f")

        reward
    }
  }

  private def hasInitiative(fakePlayingState: FakePlayingState) =
    fakePlayingState.plays.lastOption match {
      case Some(play) => play.agentId == agentId
      case None => true
    }

  /**
    * Raw combo value facts:
    * - Card values are from 1-15.
    * - Sequence length range from 5 - 12
    * - Largest single is 15
    * - Largest pair is 26
    * - Largest triplet is 39
    * - Largest sequence is 32
    * - Largest bomb is 52
    * - Two jokers is 29
    *
    * @return a value between 1 to 100, not distributed evenly
    */
  private def smartComboValue(combo: Combo): Int =
    combo.kind match {
      case SEQUENCE => combo.value + 10 // adjusted to max of 42
      case BOMB => combo.value + 40 // adjust to max of 92
      case ROCKET => 100
      case _ => combo.value
    }
}
