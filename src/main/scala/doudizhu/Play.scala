package doudizhu

case class Play(id: PlayerId, combo: Combo) extends Ordered[Play] {
  override def compare(that: Play): Int = if (id == that.id) 1 else combo.compare(that.combo)
}
