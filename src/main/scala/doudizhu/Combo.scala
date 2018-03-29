package doudizhu

object ComboKind extends Enumeration {
  type ComboKind = Value
  val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
}

import doudizhu.ComboKind._

case class Combo(cards: Cards, kind: ComboKind, value: Int) extends Ordered[Combo] {
  override def compare(that: Combo): Int =
    kind match {
      // Rocket can beat everything
      case ROCKET => 1
      // Bomb can be everything except for a rocket or a higher bomb
      case BOMB => that.kind match {
        case ROCKET => -1
        case BOMB => value - that.value
        case _ => 1
      }
      // Otherwise the play can only beat another play of the same kind and lower value
      case _ =>
        if (kind == that.kind && cards.set.size == that.cards.set.size)
          value - that.value
        else
          -1
    }

  override def toString: String = f"$kind $cards"
}

object Combo {
  def maybeCreate(cards: Cards): Option[Combo] = {
    // Values can have duplicates
    val values: List[Int] = cards.set.toList.map(_.value)
    val sameValue: Boolean = values.forall(_ == values.head)
    val maybeKind: Option[ComboKind] = cards.set.size match {
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
      case Some(kind) => Some(Combo(cards, kind, values.sum))
      case None => None
    }
  }
}