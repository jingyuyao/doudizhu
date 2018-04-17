package doudizhu

abstract class Agent(val id: PlayerId, val secret: PlayerSecret) {
  /** Returns whether to become the landlord. */
  def getAction(auctionState: AuctionState): Boolean

  /** Returns the play to make, None to pass. */
  def getAction(playingState: PlayingState): Option[Combo]

  override def toString: String = f"${this.getClass.getSimpleName}:$id"
}
