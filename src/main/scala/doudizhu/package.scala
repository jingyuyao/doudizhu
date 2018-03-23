package object doudizhu {
  /** Opaque ID of a player. */
  type PlayerId = Int
  /** Obfuscated ID used get private states related to a player. */
  type PlayerKey = Int

  case class Player(key: PlayerKey, agent: Agent)

  object PlayKind extends Enumeration {
    type PlayKind = Value
    val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
  }

}
