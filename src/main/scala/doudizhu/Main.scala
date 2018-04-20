package doudizhu

import java.io.{File, FileOutputStream, PrintStream}
import java.time.LocalDateTime

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    require(args.length >= 3)

    val agents = List(nameToAgent(args(0), 0), nameToAgent(args(1), 1), nameToAgent(args(2), 2))
    var initAgentIndex = Random.nextInt(agents.size)

    for (arg <- args.drop(3)) {
      arg match {
        case "--log" =>
          val now = LocalDateTime.now()
          val file = new File(f"logs/$now")
          file.getParentFile.mkdirs()
          file.createNewFile()
          System.setOut(new PrintStream(new FileOutputStream(file)))
        case "--init0" => initAgentIndex = 0
        case "--debug" => DEBUG = true
        case "--verbose" => VERBOSE = true
      }
    }

    Game.loop(Game.create(agents, initAgentIndex))
  }

  private def nameToAgent(name: String, id: Int): Agent = name match {
    case "human" => new HumanAgent(id, Random.nextString(10))
    case "dumb" => new DumbAgent(id, Random.nextString(10))
    case "smart" => new SmartAgent(id, Random.nextString(10))
    case _ => throw new IllegalArgumentException(f"$name is not a valid agent name")
  }
}
