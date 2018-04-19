package doudizhu

import java.io.{File, FileOutputStream, PrintStream}
import java.time.LocalDateTime

import scala.util.Random

object Main {
  private val HUMAN = false

  def main(args: Array[String]): Unit = {
    val agents = List(
      if (HUMAN) new HumanAgent(0, Random.nextString(10)) else new DumbAgent(0, Random.nextString(10)),
      new DumbAgent(1, Random.nextString(10)),
      new SmartAgent(2, Random.nextString(10))
    )

    if (!HUMAN) {
      val now = LocalDateTime.now()
      val file = new File(f"logs/$now")
      file.getParentFile.mkdirs()
      file.createNewFile()
      System.setOut(new PrintStream(new FileOutputStream(file)))
    }

    Game.loop(Game.create(agents, Random.nextInt(agents.size)))
  }
}
