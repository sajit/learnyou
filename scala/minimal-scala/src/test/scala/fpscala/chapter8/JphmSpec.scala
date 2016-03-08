package fpscala.chapter8

import fpscala.chapter6.SimpleRNG
import fpscala.BaseSpec
class JphmSpec extends BaseSpec { 
  
  it should " be an example of God's mercy on sinners" in {
    true should be(true)
  }
  
  class FooProp extends Prop {
    def check = true
  }
  it should "have a defined function check" in {
    val prop1 = new FooProp()
    prop1.check should be(true)
  }
  
  it should "choose a number less than second arg" in {
    val simpleRng = SimpleRNG(100)
    val genNum:Gen[Int] = Gen.choose2(1, 8)
    val foo = genNum.sample.run
    val (anInt,aRng) = foo.apply(simpleRng)
    anInt should be < (8)
  }
  
}