package fpscala.chapter8


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
  
  val simpleRng = RNG.Simple(100)
  
  it should "choose a number less than second arg" in {
    
    val genNum:Gen[Int] = Gen.choose2(1, 8)
    val foo = genNum.sample.run
    val (anInt,aRng) = foo.apply(simpleRng)
    anInt should be < (8)
  }
  
  it should "always get a value" in {
    val genNum:Gen[Int] = Gen.unit(3)
    val foo = genNum.sample.run
    val (anInt,aRng) = foo.apply(simpleRng)
    anInt should be (3)
  }
  
}