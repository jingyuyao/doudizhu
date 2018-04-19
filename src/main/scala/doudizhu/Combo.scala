package doudizhu

object ComboKind extends Enumeration {
  type ComboKind = Value
  val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
}

import doudizhu.ComboKind._

case class Combo(cards: Cards, kind: ComboKind, value: Int) {
  def canBeat(that: Combo): Boolean =
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

  override def toString: String = f"$kind $cards"
}

object Combo {
  /** Return a list of all possible combos from this set of cards. */
  def allFrom(cards: Cards): List[Combo] = {
    val groupedByCardValue = cards.set.groupBy(_.value).values
    // Covers SINGLE, PAIR, TRIPLET, BOMB and ROCKET
    val combosWithSameCardValue =
      groupedByCardValue
        .flatMap(cards => (1 to cards.size).map(cards.take))
        .flatMap(s => Combo.from(Cards(s)))
    val oneOfEachCardValueSorted = groupedByCardValue.map(_.head).toList.sorted
    // Covers SEQUENCE
    val sequences =
      (5 to oneOfEachCardValueSorted.size)
        .flatMap(l => oneOfEachCardValueSorted.sliding(l).filter(_.size == l))
        .flatMap(l => Combo.from(Cards(l.toSet)))
    (combosWithSameCardValue ++ sequences).toList
  }

  /** Return a combo if one can be created from this exact set of cards. */
  def from(cards: Cards): Option[Combo] = {
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
        val inSequence = cards.sorted.sliding(2).forall({ case List(left, right) => right.value - left.value == 1 })
        val noTwos = cards.set.intersect(Cards.all("2").set).isEmpty
        val noJokers = cards.set.intersect(Cards.jokers.set).isEmpty
        if (inSequence && noTwos && noJokers)
          Some(SEQUENCE)
        else
          None
      case _ => None
    }
    maybeKind match {
      case Some(kind) => Some(Combo(cards, kind, values.sum / values.size))
      case None => None
    }
  }
}
