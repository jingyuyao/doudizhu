package doudizhu

private object PlayKind extends Enumeration {
  type PlayKind = Value
  val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
}

import doudizhu.PlayKind._

case class Play(cards: Set[Card]) {
  val isValid: Boolean = ???
  private val kind: PlayKind = ???

  def canBeat(that: Play): Boolean = ???
}
