package doudizhu

import scala.util.Random

case class Game(state: State, players: List[Player], currentPlayer: Player, turn: Int) {
  require(players.size == 3)
  require(players.contains(currentPlayer))

  protected val nextPlayer: Player =
    players((players.indexWhere(_ == currentPlayer) + 1) % players.size)
}

object Game {
  def create(agents: List[Agent]): Game = {
    require(agents.size == 3)
    val players = agents.map(Player(Random.nextInt, _))
    val playerKeys = players.map(_.key)
    val playerIds = playerKeys.zipWithIndex.toMap
    val randomCards = Random.shuffle(Cards.all.sorted)
    val (chest, restCards) = randomCards.splitAt(3)
    val hands = playerKeys.zip(restCards.grouped(17).map((cards) => Cards(cards.toSet)).toList).toMap
    val initPlayer = players(Random.nextInt(players.size))
    val initState = AuctionState(hands, playerIds, Cards(chest.toSet))
    Game(initState, players, initPlayer, 0)
  }

  def loop(game: Game): Unit = {
    val currentPlayerKey = game.currentPlayer.key
    val currentAgent = game.currentPlayer.agent
    val newState: State = game.state match {
      case auctionState: AuctionState =>
        if (currentAgent.getAction(currentPlayerKey, auctionState))
          auctionState.setLandlord(currentPlayerKey)
        else if (game.turn == 2)
          auctionState.setLandlord(game.nextPlayer.key)
        else
          auctionState
      case playingState: PlayingState =>
        playingState.getWinner match {
          case Some(winner) =>
            if (winner == playingState.getLandlord)
              println(f"Landlord player $winner won!")
            else
              println(f"Peasant player $winner won!")
            return
          case None =>
            currentAgent.getAction(currentPlayerKey, playingState) match {
              case Some(play) => playingState.play(currentPlayerKey, play)
              case None => playingState
            }
        }
    }
    loop(Game(newState, game.players, game.nextPlayer, game.turn + 1))
  }
}
