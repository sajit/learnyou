package fpscala.chapter15

import fpscala.BaseSpec

/**
  * Created by sajit on 11/26/16.
  */
class ProcessSpec extends BaseSpec{

  it should "sum up values" in {
    val r = Process.sum(Stream(4.1,6.9,1.0))
    r.toList should be (List(4.1,11.0,12.0))
  }

}
