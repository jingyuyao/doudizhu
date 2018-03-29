package doudizhu

import scala.util.Random

case class Game(state: State, agents: List[Agent], currentAgent: Agent, turn: Int) {
  require(agents.size == 3)
  require(agents.contains(currentAgent))

  val nextAgent: Agent = agents((agents.indexWhere(_ == currentAgent) + 1) % agents.size)
}

object Game {
  def create(agents: List[Agent]): Game = {
    require(agents.size == 3)
    val secrets = agents.map(_.secret)
    val randomCards = Random.shuffle(Cards.all.sorted)
    val (chest, restCards) = randomCards.splitAt(3)
    val hands = secrets.zip(restCards.grouped(17).map((cards) => Cards(cards.toSet)).toList).toMap
    val secretIdMap = agents.map(a => (a.secret, a.id)).toMap
    val initState = new AuctionState(secretIdMap, hands, Cards(chest.toSet))
    val initAgent = agents(Random.nextInt(agents.size))
    Game(initState, agents, initAgent, 0)
  }

  def loop(game: Game): Unit = {
    val currentAgent = game.currentAgent
    val (nextState, nextAgent): (State, Agent) = game.state match {
      case auctionState: AuctionState =>
        if (currentAgent.getAction(auctionState))
          (auctionState.setLandlord(currentAgent.secret), currentAgent)
        else if (game.turn == 2)
          (auctionState.setLandlord(game.nextAgent.secret), game.nextAgent)
        else
          (auctionState, game.nextAgent)
      case playingState: PlayingState =>
        playingState.winner match {
          case Some(winner) =>
            if (winner == playingState.landlord)
              println(f"Landlord player $winner won!")
            else
              println(f"Peasant player $winner won!")
            return
          case None =>
            (currentAgent.getAction(playingState) match {
              case Some(play) => playingState.play(currentAgent.secret, play)
              case None => playingState
            }, game.nextAgent)
        }
    }
    loop(Game(nextState, game.agents, nextAgent, game.turn + 1))
  }
}
