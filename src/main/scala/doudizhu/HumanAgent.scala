package doudizhu

import scala.io.StdIn.readLine

class HumanAgent extends Agent {
  /** Returns whether to become the landlord. */
  override def getAction(playerKey: PlayerKey, auctionState: AuctionState): Boolean = {
    printBanner()
    println("Auctioning...")
    println(f"Your hand: ${auctionState.getHand(playerKey)}")
    println("Accept landlord? [Y/N]")
    readToUpper match {
      case "Y" => true
      case "N" => false
      case _ =>
        println("Invalid input")
        getAction(playerKey, auctionState)
    }
  }

  /** Returns the play to make, None to pass. */
  override def getAction(playerKey: PlayerKey, playingState: PlayingState): Option[Play] = {
    printBanner()
    println(f"You are player ${playingState.getPlayerId(playerKey)}")
    println(f"Your hand: ${playingState.getHand(playerKey)}")
    playingState.getPlays.lastOption match {
      case Some(play) => println(f"Last play by player ${play._1}: ${play._2}")
      case None => println("Play anything")
    }
    println("Your action [PASS/INFO/PLAYS/Cards to play]")
    readToUpper match {
      case "PASS" => None
      case "INFO" =>
        println(f"Landlord is player ${playingState.getLandlord}")
        getAction(playerKey, playingState)
      case "PLAYS" =>
        playingState.getPlays.foreach((p) => println(f"Player ${p._1}: ${p._2}"))
        getAction(playerKey, playingState)
      case input =>
        val selections = input.split(" ").filter(_.nonEmpty).map(_.trim)
        val hand = playingState.getHand(playerKey)
        val selectedCards = hand(selections: _*)
        Play.maybeCreate(selectedCards) match {
          case Some(play) =>
            if (playingState.isValid(playerKey, play)) {
              println(f"Play $play [Y/N]")
              readToUpper match {
                case "Y" => Some(play)
                case "N" => getAction(playerKey, playingState)
                case _ =>
                  println("Invalid input")
                  getAction(playerKey, playingState)
              }
            }
            else {
              println(f"Your play cannot beat the previous play: $play")
              getAction(playerKey, playingState)
            }
          case None =>
            println(f"The cards you picked is not a valid play: $selectedCards")
            getAction(playerKey, playingState)
        }
    }
  }

  private def printBanner(): Unit = println("----------------------------------------")

  private def readToUpper = readLine().trim.toUpperCase
}
