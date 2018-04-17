package doudizhu

/**
  * A stupid agent that always accepts landlord and always try to beat the last play with the
  * weakest combo possible.
  */
class DumbAgent(id: PlayerId, secret: PlayerSecret) extends Agent(id, secret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = true

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val hand = playingState.getHand(secret)
    val combos = Combo.allFrom(hand).sortWith(dumbComboOrdering)
    combos.find(combo => playingState.isValid(secret, combo))
  }

  private def dumbComboOrdering(l: Combo, r: Combo): Boolean =
    if (r.canBeat(l)) true else r.kind > l.kind
}
