package doudizhu

import org.scalatest.FlatSpec

class CardSpec extends FlatSpec {

  "Cards" should "be 54 in total" in {
    assert(Cards.all.set.size == 54)
  }

}
