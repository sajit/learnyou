package fpscala.chapter10

import fpscala.BaseSpec
class Chapter10Spec extends BaseSpec{
  
  it should "behave like a monoid" in {
    MonoidTypes.stringMonoid.zero should be ("")
    MonoidTypes.stringMonoid.op("hello", "world") should be ("helloworld")
    

  }
  
}