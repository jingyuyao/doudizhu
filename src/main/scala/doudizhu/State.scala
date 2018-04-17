package doudizhu

/** Never leak secrets. Prefer storing ID instead of secrets. */
class State(secretIdMap: Map[AgentSecret, AgentId], hands: Map[AgentSecret, Cards]) {
  def getAllAgentIds: List[AgentId] = secretIdMap.values.toList

  def getAgentId(agentSecret: AgentSecret): AgentId = secretIdMap(agentSecret)

  def getHand(agentSecret: AgentSecret): Cards = hands(agentSecret)

  def getNumCardsInHand: Map[AgentId, Int] =
    hands.map({ case (secret, cards) => (getAgentId(secret), cards.set.size) })

  def otherCardsInPlay(agentSecret: AgentSecret): Cards =
    Cards(hands.filterKeys(_ != agentSecret).values.flatMap(_.set).toSet)

  def getWinner: Option[AgentId] = getNumCardsInHand.find(_._2 == 0).map(_._1)
}
