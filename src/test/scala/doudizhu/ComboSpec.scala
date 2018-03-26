package doudizhu

import doudizhu.Cards.{all, jokers}
import doudizhu.PlayKind._
import org.scalatest.FlatSpec

class ComboSpec extends FlatSpec {

  "Two jokers" should "be a rocket" in {
    Combo.maybeCreate(jokers) match {
      case Some(play) => assert(play.kind == ROCKET)
      case _ => fail()
    }
  }

  "Four fours" should "be a bomb" in {
    Combo.maybeCreate(all("4")) match {
      case Some(play) => assert(play.kind == BOMB)
      case _ => fail()
    }
  }

  "Three three" should "be a triplet" in {
    Combo.maybeCreate(all("3").take(3)) match {
      case Some(play) => assert(play.kind == TRIPLET)
      case _ => fail()
    }
  }

  "Two twos" should "be a pair" in {
    Combo.maybeCreate(all("2").take(2)) match {
      case Some(play) => assert(play.kind == PAIR)
      case _ => fail()
    }
  }

  "One ace" should "be a single" in {
    Combo.maybeCreate(all("A").take(1)) match {
      case Some(play) => assert(play.kind == SINGLE)
      case _ => fail()
    }
  }

  "3 to 7" should "be a sequence" in {
    Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+")) match {
      case Some(play) => assert(play.kind == SEQUENCE)
      case _ => fail()
    }
  }

  "Random numbers" should "not be a play" in {
    Combo.maybeCreate(all("3+", "5+", "7+", "8+", "10+")) match {
      case Some(_) => fail()
      case _ => succeed
    }
  }

  "4 to 8" should "beat 3 to 7" in {
    val threeToSeven = Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+")).get
    val fourToEight = Combo.maybeCreate(all("4+", "5+", "6+", "7+", "8+")).get
    assert(fourToEight.canBeat(threeToSeven))
    assert(!threeToSeven.canBeat(fourToEight))
  }

  "3 to 8" should "not beat 3 to 7" in {
    val threeToEight = Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+", "8+")).get
    val threeToSeven = Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+")).get
    assert(!threeToSeven.canBeat(threeToEight))
    assert(!threeToEight.canBeat(threeToSeven))
  }

  "Rocket" should "beat everything" in {
    val rocket = Combo.maybeCreate(jokers).get
    val single = Combo.maybeCreate(all("A").take(1)).get
    val double = Combo.maybeCreate(all("A").take(2)).get
    val triplet = Combo.maybeCreate(all("A").take(3)).get
    val bomb = Combo.maybeCreate(all("A").take(4)).get
    val sequence = Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+")).get
    assert(rocket.canBeat(single))
    assert(rocket.canBeat(double))
    assert(rocket.canBeat(triplet))
    assert(rocket.canBeat(bomb))
    assert(rocket.canBeat(single))
  }

  "Bomb" should "beat everything except for rocket" in {
    val rocket = Combo.maybeCreate(jokers).get
    val single = Combo.maybeCreate(all("A").take(1)).get
    val double = Combo.maybeCreate(all("A").take(2)).get
    val triplet = Combo.maybeCreate(all("A").take(3)).get
    val bomb = Combo.maybeCreate(all("A").take(4)).get
    val bomb2 = Combo.maybeCreate(all("2").take(4)).get
    val sequence = Combo.maybeCreate(all("3+", "4+", "5+", "6+", "7+")).get
    assert(!bomb.canBeat(rocket))
    assert(bomb.canBeat(double))
    assert(bomb.canBeat(triplet))
    assert(!bomb.canBeat(bomb2))
    assert(bomb2.canBeat(bomb))
    assert(bomb.canBeat(single))
  }
}
