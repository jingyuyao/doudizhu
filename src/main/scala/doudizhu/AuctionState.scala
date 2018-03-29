package doudizhu

class AuctionState(secretIdMap: Map[PlayerSecret, PlayerId],
                   hands: Map[PlayerSecret, Cards],
                   chest: Cards) extends State(secretIdMap, hands) {
  require(chest.set.size == 3)
  require(hands.values.map(_.set.size).forall(_ == 17))
  // Contains exactly one copy of each card.
  require({
    val cardsInHand = hands.values.map(_.set.toList).fold(List())(_ ++ _)
    val cardsInState = cardsInHand ++ chest.set.toList
    Cards.all.set.size == cardsInState.size && Cards.all.set == cardsInState.toSet
  })

  def setLandlord(secret: PlayerSecret): PlayingState = {
    val landlordHand = Cards(getHand(secret).set.union(chest.set))
    val startingHands = hands.updated(secret, landlordHand)
    new PlayingState(secretIdMap, startingHands, startingHands, getPlayerId(secret), List())
  }
}
