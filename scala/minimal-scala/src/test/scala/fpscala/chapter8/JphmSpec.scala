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
  
}