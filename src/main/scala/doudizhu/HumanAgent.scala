package doudizhu

import scala.io.StdIn.readLine

class HumanAgent(agentId: AgentId, agentSecret: AgentSecret) extends Agent(agentId, agentSecret) {
  /** Returns whether to become the landlord. */
  override def getAction(auctionState: AuctionState): Boolean = {
    println(f"Your hand: ${auctionState.getHand(agentSecret)}")
    print("Accept landlord? [Y/N] ")
    readToUpper match {
      case "Y" => true
      case "N" => false
      case _ =>
        println("Invalid input")
        getAction(auctionState)
    }
  }

  /** Returns the play to make, None to pass. */
  override def getAction(playingState: PlayingState): Option[Combo] = {
    println(f"Your hand: ${playingState.getHand(agentSecret)}")
    print("Your action: [PASS/INFO/PLAYS/Cards to play] ")
    readToUpper match {
      case "PASS" => None
      case "INFO" =>
        println(f"Landlord is agent ${playingState.landlord}")
        playingState.getNumCardsInHand.toList.sortBy(_._1)
          .foreach({ case (i, n) => println(f"Agent $i has $n cards") })
        println(f"Unseen cards: ${playingState.getUnseenCards(agentSecret)}")
        getAction(playingState)
      case "PLAYS" =>
        playingState.plays.foreach((p) => println(f"Agent ${p.agentId}: ${p.combo}"))
        getAction(playingState)
      case input =>
        val selections = input.split(" ").filter(_.nonEmpty).map(_.trim)
        val hand = playingState.getHand(agentSecret)
        val selectedCards = hand(selections: _*)
        Combo.from(selectedCards) match {
          case Some(play) =>
            if (playingState.isValid(agentSecret, play)) {
              print(f"Play $play? [Y/N] ")
              readToUpper match {
                case "Y" => Some(play)
                case "N" => getAction(playingState)
                case _ =>
                  println("Invalid input")
                  getAction(playingState)
              }
            }
            else {
              println(f"Your play cannot beat the previous play: $play")
              getAction(playingState)
            }
          case None =>
            println(f"The cards you picked is not a valid play: $selectedCards")
            getAction(playingState)
        }
    }
  }

  private def readToUpper = readLine().trim.toUpperCase
}
