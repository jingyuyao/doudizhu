package doudizhu

/** Never leak secrets. Prefer storing ID instead of secrets. */
trait State {
  protected val hands: Map[PlayerSecret, Cards]
  protected val secretIdMap: Map[PlayerSecret, PlayerId]

  lazy val numCardsInHand: Map[PlayerId, Int] =
    hands.map({ case (secret, cards) => (getPlayerId(secret), cards.set.size) })

  lazy val winner: Option[PlayerId] = numCardsInHand.find(_._2 == 0).map(_._1)

  def getHand(secret: PlayerSecret): Cards = hands(secret)

  def getPlayerId(secret: PlayerSecret): PlayerId = secretIdMap(secret)
}
