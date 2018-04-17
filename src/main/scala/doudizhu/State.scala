package doudizhu

/** Never leak secrets. Prefer storing ID instead of secrets. */
class State(secretIdMap: Map[PlayerSecret, PlayerId], hands: Map[PlayerSecret, Cards]) {
  def getPlayerId(secret: PlayerSecret): PlayerId = secretIdMap(secret)

  def getHand(secret: PlayerSecret): Cards = hands(secret)

  def getNumCardsInHand: Map[PlayerId, Int] =
    hands.map({ case (secret, cards) => (getPlayerId(secret), cards.set.size) })

  def getWinner: Option[PlayerId] = getNumCardsInHand.find(_._2 == 0).map(_._1)
}
