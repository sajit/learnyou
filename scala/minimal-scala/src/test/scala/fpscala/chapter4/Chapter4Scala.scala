package fpscala.chapter4

import fpscala.chapter3.{BList, BNil, BCons}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created
 * by sajit on 9/19/15.
 */
class Chapter4Scala extends FlatSpec with Matchers{

  it  should " meh " in {
    BSome(49).get should be (49)
  }


}
