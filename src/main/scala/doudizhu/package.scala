package object doudizhu {
  /** Opaque ID of an agent. */
  type AgentId = Int
  /** Secret used get private states related to an agent. */
  type AgentSecret = String

  // Terrible global variable practice but meh.
  var DEBUG: Boolean = false
  var VERBOSE: Boolean = false
}
