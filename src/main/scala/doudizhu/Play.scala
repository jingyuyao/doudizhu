package doudizhu

import doudizhu.PlayKind._

case class Play(cards: Cards, kind: PlayKind, value: Int) {
  def canBeat(that: Play): Boolean = {
    kind match {
      // Rocket can beat everything
      case ROCKET => true
      // Bomb can be everything except for a rocket or a higher bomb
      case BOMB => that.kind match {
        case ROCKET => false
        case BOMB => value > that.value
        case _ => true
      }
      // Otherwise the play can only beat another play of the same kind and lower value
      case _ => kind == that.kind && cards.set.size == that.cards.set.size && value > that.value
    }
  }

  override def toString: String = f"$kind $cards"
}

object Play {
  def maybeCreate(cards: Cards): Option[Play] = {
    // Values can have duplicates
    val values: List[Int] = cards.set.toList.map(_.value)
    val sameValue: Boolean = values.forall(_ == values.head)
    val maybeKind: Option[PlayKind] = cards.set.size match {
      case 1 => Some(SINGLE)
      case 2 if cards == Cards.jokers => Some(ROCKET)
      case 2 if sameValue => Some(PAIR)
      case 3 if sameValue => Some(TRIPLET)
      case 4 if sameValue => Some(BOMB)
      case x if x >= 5 =>
        val max = values.max
        val min = values.min
        val inSequence = max - min == values.size - 1 && values.toSet == (min to max).toSet
        val noTwos = cards.set.intersect(Cards.all("2").set).isEmpty
        val noJokers = cards.set.intersect(Cards.jokers.set).isEmpty
        if (inSequence && noTwos && noJokers)
          Some(SEQUENCE)
        else
          None
      case _ => None
    }
    maybeKind match {
      case Some(kind) => Some(Play(cards, kind, values.sum))
      case None => None
    }
  }
}
