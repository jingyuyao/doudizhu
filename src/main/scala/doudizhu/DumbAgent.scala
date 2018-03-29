package doudizhu

class DumbAgent(override val id: PlayerId, override val secret: PlayerSecret) extends Agent(id, secret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = true

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    val hand = playingState.getHand(secret)
    val combos = Combo.allFrom(hand)
    playingState.plays.lastOption match {
      case Some(play) => combos.find(combo => Play(id, combo) > play)
      case None => Some(combos.head)
    }
  }
}
