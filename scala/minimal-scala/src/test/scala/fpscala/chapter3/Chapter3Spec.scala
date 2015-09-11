package fpscala.chapter3

import org.scalatest.{Matchers, FlatSpec}


/**
 * Created by sajit on 9/9/15.
 */
class Chapter3Spec extends FlatSpec with Matchers {


    "foldright " should " apply a function " in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      val sum = BList.foldRight(bList,0)((x,y) => x + y)
      sum should be === (14)
    }

    "length" should " calculate length" in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      BList.length(bList) should be (4)
    }


}
