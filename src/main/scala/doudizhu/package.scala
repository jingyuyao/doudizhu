package object doudizhu {
  /** Opaque ID of a player. */
  type PlayerId = Int
  /** Secret used get private states related to a player. */
  type PlayerSecret = String

  case class Play(id: PlayerId, combo: Combo)

  object PlayKind extends Enumeration {
    type PlayKind = Value
    val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
  }

}
