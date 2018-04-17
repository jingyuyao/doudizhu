package doudizhu

case class Play(agentId: AgentId, combo: Combo) {
  def canBeat(that: Play): Boolean = agentId == that.agentId || combo.canBeat(that.combo)
}
