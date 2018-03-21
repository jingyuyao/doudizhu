package doudizhu

import doudizhu.PlayKind._
import org.scalatest.FlatSpec

class PlaySpec extends FlatSpec {

  "Two jokers" should "be a rocket" in {
    Play.make(Card.jokers) match {
      case Some(play) => assert(play.kind == ROCKET)
      case _ => fail()
    }
  }

  "Four fours" should "be a bomb" in {
    Play.make(Card.get("4")) match {
      case Some(play) => assert(play.kind == BOMB)
      case _ => fail()
    }
  }

  "Three three" should "be a triplet" in {
    Play.make(Card.get("3").take(3)) match {
      case Some(play) => assert(play.kind == TRIPLET)
      case _ => fail()
    }
  }

  "Two twos" should "be a pair" in {
    Play.make(Card.get("2").take(2)) match {
      case Some(play) => assert(play.kind == PAIR)
      case _ => fail()
    }
  }

  "One ace" should "be a single" in {
    Play.make(Card.get("A").take(1)) match {
      case Some(play) => assert(play.kind == SINGLE)
      case _ => fail()
    }
  }

  "1 to 5" should "be a sequence" in {
    Play.make(Set(Card(1, "a"), Card(2, "b"), Card(3, "c"), Card(4, "c"), Card(5, "c"))) match {
      case Some(play) => assert(play.kind == SEQUENCE)
      case _ => fail()
    }
  }

  "Random numbers" should "not be a play" in {
    Play.make(Set(Card(1, "a"), Card(2, "b"), Card(3, "c"), Card(7, "c"), Card(9, "c"))) match {
      case Some(_) => fail()
      case _ => succeed
    }
  }

  "2 to 6" should "beat 1 to 5" in {
    val oneToFive = Play.make(Set(Card(1, "a"), Card(2, "b"), Card(3, "c"), Card(4, "c"), Card(5, "c"))).get
    val twoToSix = Play.make(Set(Card(2, "b"), Card(3, "c"), Card(4, "c"), Card(5, "c"), Card(6, "a"))).get
    assert(twoToSix.canBeat(oneToFive))
    assert(!oneToFive.canBeat(twoToSix))
  }

  "2 to 7" should "not beat 1 to 5" in {
    val oneToFive = Play.make(Set(Card(1, "a"), Card(2, "b"), Card(3, "c"), Card(4, "c"), Card(5, "c"))).get
    val twoToSix = Play.make(Set(Card(2, "b"), Card(3, "c"), Card(4, "c"), Card(5, "c"), Card(6, "a"), Card(7, "a"))).get
    assert(!twoToSix.canBeat(oneToFive))
    assert(!oneToFive.canBeat(twoToSix))
  }
}
