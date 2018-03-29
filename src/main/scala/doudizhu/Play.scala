package doudizhu

case class Play(id: PlayerId, combo: Combo) {
  def canBeat(that: Play): Boolean = id == that.id || combo.canBeat(that.combo)
}
