package doudizhu

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val agents = List(
      new DumbAgent(0, Random.nextString(10)),
      new DumbAgent(1, Random.nextString(10)),
      new SmartAgent(2, Random.nextString(10))
    )
//    val initAgentIndex = 0
    val initAgentIndex = Random.nextInt(agents.size)
    Game.loop(Game.create(agents, initAgentIndex))
  }
}
