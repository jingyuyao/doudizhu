package doudizhu

import scala.util.Random

case class Game(state: State, agents: List[Agent], currentAgent: Agent, turn: Int) {
  require(agents.size == 3)
  require(agents.contains(currentAgent))

  val nextAgent: Agent = agents((agents.indexWhere(_ == currentAgent) + 1) % agents.size)

  def getAgent(id: PlayerId): Agent = agents.find(_.id == id).get
}

object Game {
  private val BANNER_SIZE = 40

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
    println("-" * BANNER_SIZE)

    val currentAgent = game.currentAgent
    println(f"Turn ${game.turn} with: $currentAgent")

    val (nextState, nextAgent): (State, Agent) = game.state match {
      case auctionState: AuctionState =>
        if (currentAgent.getAction(auctionState)) {
          println(f"$currentAgent accepted landlord")
          (auctionState.setLandlord(currentAgent.secret), currentAgent)
        }
        else if (game.turn == 2) {
          println(f"$currentAgent passed landlord")
          println(f"Landlord defaulted to ${game.nextAgent}")
          (auctionState.setLandlord(game.nextAgent.secret), game.nextAgent)
        }
        else {
          println(f"$currentAgent passed landlord")
          (auctionState, game.nextAgent)
        }
      case playingState: PlayingState =>
        playingState.plays.lastOption match {
          case Some(play) => println(f"Last play: ${game.getAgent(play.id)} -> ${play.combo}")
          case None => println("New game")
        }
        playingState.getWinner match {
          case Some(winner) =>
            if (winner == playingState.landlord)
              println(f"Landlord ${game.getAgent(winner)} won!")
            else
              println(f"Peasant ${game.getAgent(winner)} won!")
            return
          case None =>
            (currentAgent.getAction(playingState) match {
              case Some(play) =>
                println(f"$currentAgent played $play")
                playingState.play(currentAgent.secret, play)
              case None =>
                println(f"$currentAgent passed")
                playingState
            }, game.nextAgent)
        }
    }
    loop(Game(nextState, game.agents, nextAgent, game.turn + 1))
  }
}
