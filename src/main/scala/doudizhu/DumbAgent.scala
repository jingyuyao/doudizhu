package doudizhu

/**
  * A stupid agent that always rejects landlord and always try to beat the last play with the
  * weakest combo possible.
  */
class DumbAgent(agentId: AgentId, agentSecret: AgentSecret) extends Agent(agentId, agentSecret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = false

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] =
    Combo
      .allFrom(playingState.getHand(agentSecret))
      .toList
      .sortWith(dumbComboOrdering)
      .find(combo => playingState.isValid(agentSecret, combo))

  private def dumbComboOrdering(l: Combo, r: Combo): Boolean =
    if (r.canBeat(l)) true else r.kind > l.kind
}
