package doudizhu

case class AuctionState(protected val hands: Map[PlayerSecret, Cards],
                        protected val secretIdMap: Map[PlayerSecret, PlayerId],
                        private val chest: Cards) extends State {
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
    PlayingState(startingHands, secretIdMap, getPlayerId(secret), List(), startingHands)
  }
}
