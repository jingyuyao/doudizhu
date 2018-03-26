package doudizhu

import scala.util.Random

object DouDiZhu {
  def main(args: Array[String]): Unit = {
    val agents = List(
      new HumanAgent(0, Random.nextString(10)),
      new HumanAgent(1, Random.nextString(10)),
      new HumanAgent(2, Random.nextString(10))
    )
    Game.loop(Game.create(agents))
  }
}
