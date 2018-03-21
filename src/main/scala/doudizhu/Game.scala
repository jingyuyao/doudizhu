package doudizhu

import scala.io.StdIn.readLine
import scala.util.Random

object Game {
  def create(): AuctionState = {
    val randomCards = Random.shuffle(Card.sorted)
    val (chest, restCards) = randomCards.splitAt(3)
    val hands = restCards.grouped(restCards.size / State.players.size).map(_.toSet).toList
    val handsMap = State.players.zip(hands).toMap
    val initialPick = State.players(Random.nextInt(State.players.size))
    AuctionState(initialPick, handsMap, initialPick, chest.toSet)
  }

  def loop(state: State): Unit = {
    val newState: State = state match {
      case auction: AuctionState =>
        println(auction)
        println("Accept landlord? [Y/N]")
        readLine().trim.toUpperCase match {
          case "Y" => auction.acceptLandlord()
          case "N" => auction.pass()
          case _ =>
            println("Invalid input")
            auction
        }
      case playing: PlayingState =>
        playing.winner match {
          case Some(winner) =>
            if (winner == playing.landlord)
              println(f"Landlord $winner won!")
            else
              println(f"Peasant $winner won!")
            return
          case None =>
            println(playing)
            println("Enter space separated card names to play, names can be partially matched, empty to pass")
            val input = readLine().trim.toUpperCase
            if (input.isEmpty) {
              playing.pass()
            }
            else {
              val cardNames = input.split(" ").filter(_.nonEmpty).map(_.trim)
              val cards = cardNames.flatMap((name) => Card.get(playing.currentHand, name)).toSet
              Play.make(cards) match {
                case Some(play) =>
                  playing.play(play) match {
                    case Some(newPlaying) => newPlaying
                    case None =>
                      println(f"Your play can't beat the last one: ${Card.sortedString(play.cards)}")
                      playing
                  }
                case None =>
                  println(f"The cards you picked is not a valid play: ${Card.sortedString(cards)}")
                  playing
              }
            }
        }
    }
    println("-------------------------------")
    loop(newState)
  }
}
