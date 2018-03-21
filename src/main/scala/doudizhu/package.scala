package object doudizhu {
  type Player = Int

  object PlayKind extends Enumeration {
    type PlayKind = Value
    val SINGLE, PAIR, TRIPLET, SEQUENCE, BOMB, ROCKET = Value
  }

}
