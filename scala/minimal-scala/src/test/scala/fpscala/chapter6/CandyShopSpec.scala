package fpscala.chapter6

import fpscala.BaseSpec

/**
 * Created by sajit.kunnumkal on 11/25/2015.
 */
class CandyShopSpec extends BaseSpec {

  it should "transition if locked and coined" in {

    val machine = Machine(true, 10, 0)
    val newState = machine.transition(Coin)
    newState.locked should be(false)
    newState.coins should be(1)
    newState.candies should be(10)
  }
  it should "not change state if unlocked and coined" in {

    val machine = Machine(false, 10, 0)
    val newState = machine.transition(Coin)
    newState.locked should be(false)
    newState.coins should be(1)
    newState.candies should be(10)
  }
  it should "not change state if locked and turned" in {

    val machine = Machine(true, 10, 0)
    val newState = machine.transition(Turn)
    newState.locked should be(true)
    newState.coins should be(0)
    newState.candies should be(10)
  }
  it should "dispense candy if unlocked and turned" in {

    val machine = Machine(false, 10, 0)
    val newState = machine.transition(Turn)
    newState.locked should be(true)
    newState.coins should be(0)
    newState.candies should be(9)
  }
}
