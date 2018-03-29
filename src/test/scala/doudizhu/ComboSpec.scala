package doudizhu

import doudizhu.Cards.{all, jokers}
import doudizhu.ComboKind._
import org.scalatest.FlatSpec

class ComboSpec extends FlatSpec {

  "Two jokers" should "be a rocket" in {
    Combo.from(jokers) match {
      case Some(play) => assert(play.kind == ROCKET)
      case _ => fail()
    }
  }

  "Four fours" should "be a bomb" in {
    Combo.from(all("4")) match {
      case Some(play) => assert(play.kind == BOMB)
      case _ => fail()
    }
  }

  "Three three" should "be a triplet" in {
    Combo.from(all("3").take(3)) match {
      case Some(play) => assert(play.kind == TRIPLET)
      case _ => fail()
    }
  }

  "Two twos" should "be a pair" in {
    Combo.from(all("2").take(2)) match {
      case Some(play) => assert(play.kind == PAIR)
      case _ => fail()
    }
  }

  "One ace" should "be a single" in {
    Combo.from(all("A").take(1)) match {
      case Some(play) => assert(play.kind == SINGLE)
      case _ => fail()
    }
  }

  "3 to 7" should "be a sequence" in {
    Combo.from(all("3+", "4+", "5+", "6+", "7+")) match {
      case Some(play) => assert(play.kind == SEQUENCE)
      case _ => fail()
    }
  }

  "Random numbers" should "not be a play" in {
    Combo.from(all("3+", "5+", "7+", "8+", "10+")) match {
      case Some(_) => fail()
      case _ => succeed
    }
  }

  "4 to 8" should "beat 3 to 7" in {
    val threeToSeven = Combo.from(all("3+", "4+", "5+", "6+", "7+")).get
    val fourToEight = Combo.from(all("4+", "5+", "6+", "7+", "8+")).get
    assert(fourToEight > threeToSeven)
    assert(threeToSeven < fourToEight)
  }

  "3 to 8" should "not beat 3 to 7" in {
    val threeToEight = Combo.from(all("3+", "4+", "5+", "6+", "7+", "8+")).get
    val threeToSeven = Combo.from(all("3+", "4+", "5+", "6+", "7+")).get
    assert(threeToSeven < threeToEight)
    assert(threeToEight < threeToSeven)
  }

  "Rocket" should "beat everything" in {
    val rocket = Combo.from(jokers).get
    val single = Combo.from(all("A").take(1)).get
    val double = Combo.from(all("A").take(2)).get
    val triplet = Combo.from(all("A").take(3)).get
    val bomb = Combo.from(all("A").take(4)).get
    val sequence = Combo.from(all("3+", "4+", "5+", "6+", "7+")).get
    assert(rocket > single)
    assert(rocket > double)
    assert(rocket > triplet)
    assert(rocket > bomb)
    assert(rocket > single)
    assert(rocket > sequence)
  }

  "Bomb" should "beat everything except for rocket" in {
    val rocket = Combo.from(jokers).get
    val single = Combo.from(all("A").take(1)).get
    val double = Combo.from(all("A").take(2)).get
    val triplet = Combo.from(all("A").take(3)).get
    val bomb = Combo.from(all("A").take(4)).get
    val bomb2 = Combo.from(all("2").take(4)).get
    val sequence = Combo.from(all("3+", "4+", "5+", "6+", "7+")).get
    assert(bomb < rocket)
    assert(bomb > double)
    assert(bomb > triplet)
    assert(bomb < bomb2)
    assert(bomb2 > bomb)
    assert(bomb > single)
    assert(bomb > sequence)
  }
}
