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
  private val numGetSuccessor = new AtomicInteger()
  private val numEval = new AtomicInteger()
  private val numSmartActions = new AtomicInteger()
  private val numLegalActions = new AtomicInteger()
  private val numAllActions = new AtomicInteger()

  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = {
    val hand = auctionState.getHand(agentSecret)
    val handCombos = hand.getAllCombo
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
        val nextAgent = getNextAgent(fakePlayingState, agentId)
        val comboValues = comboSuccessorPairs.map({ case (combo, state) => (combo, expectiMax(state, 0, nextAgent)) })
        val maxComboValue = comboValues.maxBy(_._2)
        val currentStateValue = eval(fakePlayingState)
        if (DEBUG) println(f"    current state $currentStateValue%.2f, max combo ${maxComboValue._2}%.2f ${maxComboValue._1}")
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

  private def expectiMax(fakePlayingState: FakePlayingState, currentDepth: Int, currentAgent: AgentId): Double =
    if (isTerminal(fakePlayingState, currentDepth)) {
      eval(fakePlayingState)
    } else {
      val successorStates = getSuccessorStates(fakePlayingState, currentAgent)
      val nextAgent = getNextAgent(fakePlayingState, currentAgent)
      if (isSameTeam(fakePlayingState, currentAgent)) {
        val minValues = successorStates.map(state => expectiMax(state, currentDepth, nextAgent))
        minValues.max
      } else {
        val maxValues =
          if (isSameTeam(fakePlayingState, nextAgent)) {
            val nextDepth = currentDepth + 1
            successorStates.map(state => expectiMax(state, nextDepth, nextAgent))
          }
        else
        {
          successorStates.map(state => expectiMax(state, currentDepth, nextAgent))
        }
        maxValues
        .sum / maxValues.size
      }
    }

  private def isTerminal(fakePlayingState: FakePlayingState, currentDepth: Int): Boolean =
    currentDepth == maxDepth || fakePlayingState.getWinner.nonEmpty

  private def isSameTeam(fakePlayingState: FakePlayingState, currentAgent: AgentId): Boolean =
    currentAgent == agentId || (agentId != fakePlayingState.landlord && currentAgent != fakePlayingState.landlord)

  private def getNextAgent(fakePlayingState: FakePlayingState, currentAgent: AgentId): AgentId = {
    val agents = fakePlayingState.getAllAgentIds
    agents((agents.indexWhere(_ == currentAgent) + 1) % agents.size)
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
      if (isSameTeam(fakePlayingState, id))
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
      else if (isSameTeam(fakePlayingState, id))
        Cards(Set()) // Assume teammate would not play against you.
      else
        fakePlayingState.getUnseenCards(agentSecret)
    val allActions = availableCards.getAllCombo
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
        val handCombos = hand.getAllCombo
        val handComboValuesSorted = handCombos.map(smartComboValue).toList.sorted

        val numCardsInHandFeature = 200.0 / hand.set.size
        val medianHandComboValueFeature =
          if (handComboValuesSorted.size % 2 == 1) {
            handComboValuesSorted(handComboValuesSorted.size / 2).toDouble
          }
          else {
            val (up, down) = handComboValuesSorted.splitAt(handComboValuesSorted.size / 2)
            (up.last + down.head) / 2.0
          }
        val handComboKindsFeature = handCombos.map(_.kind).toSet.size

        val reward = numCardsInHandFeature + medianHandComboValueFeature + handComboKindsFeature

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
    * Raw combo values are average of the cards which ranges from 1-15.
    *
    * @return a value between 1 to 100, not distributed evenly
    */
  private def smartComboValue(combo: Combo): Int =
    combo.kind match {
      case SINGLE => combo.value // 1-15
      case PAIR => combo.value + 1 // 2-16
      case TRIPLET => combo.value + 2 // 3-17
      case SEQUENCE => combo.value + 3 // 4-18
      case BOMB => combo.value + 50 // 51-55
      case ROCKET => 100
    }
}
