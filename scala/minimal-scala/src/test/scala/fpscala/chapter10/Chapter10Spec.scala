package fpscala.chapter10

import fpscala.BaseSpec
class Chapter10Spec extends BaseSpec{ 
  
  it should "behave like a monoid" in {
    MonoidTypes.stringMonoid.zero should be ("")
    MonoidTypes.stringMonoid.op("hello", "world") should be ("helloworld")
    

  }
  
  it should "boolean Andify a Monoid" in {
    MonoidTypes.booleanAnd.zero should be (true)
    MonoidTypes.booleanAnd.op(true, false) should be (false)
  }
  
  it should "identify function a Monoid" in {
    val endMonoid:Monoid[Int => Int] = MonoidTypes.endoMonoid[Int]
    endMonoid.zero(5) should be (5)
    endMonoid.op({x => x + 1}, {x => x + 2})(1) should be (4)
    endMonoid.op({x=> x + 2},{x => x + 1})(1) should be (4)
  }

  
  
}