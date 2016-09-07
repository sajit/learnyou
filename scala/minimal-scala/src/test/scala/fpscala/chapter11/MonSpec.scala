package fpscala.chapter11

import fpscala.BaseSpec

class MonSpec extends BaseSpec{
  
  it should "map options " in {
    val intOption = MonUtils.optionMon.map(Some("alfredo"))(st => st.length())
    intOption should be (Some(7))
  }
}