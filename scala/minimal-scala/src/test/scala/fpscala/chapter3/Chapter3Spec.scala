package fpscala.chapter3

import org.scalatest.{Matchers, FlatSpec}


/**
 * Created by sajit on 9/9/15.
 */
class Chapter3Spec extends FlatSpec with Matchers {


    it  should " apply a function in foldRight " in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      val sum = BList.foldRight(bList,0)((x,y) => x + y)
      sum should be === (14)
    }

   it should " calculate length" in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      BList.length(bList) should be (4)
    }
    it should "fold left sum" in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      val sum = BList.foldLeft(bList,0)((x,y) => x + y)
      sum should be === (14)
    }

  it should "fold left product" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val product = BList.foldLeft(bList,1)((x,y) => x * y)
    product should be === (-120)
  }

  it should "reverse" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val reverse = BList.reverse(bList)
    BList.length(reverse) should be === (BList.length(bList))
    reverse should be === (BCons(2,BCons(3,BCons(-5,BCons(4,BNil)))))
  }

  it should "reverse from a foldLeft" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val reverse = BList.revV2(bList)
    BList.length(reverse) should be === (BList.length(bList))
    reverse should be === (BCons(2,BCons(3,BCons(-5,BCons(4,BNil)))))
  }

  it should "append using foldRight" in {
    val a1 = BCons(2,BCons(4,BCons(6,BNil)))
    val a2 = BCons(-3,BCons(-5,BNil))
    val result = BList.appendv2(a1,a2)
    BList.length(result) should be === (5)
    result should be === (BCons(2,BCons(4,BCons(6,BCons(-3,BCons(-5,BNil))))))
  }

  it should "flatten a list of lists " in {
    val a1 = BCons(2,BCons(4,BCons(6,BNil)))
    val a2 = BCons(-3,BCons(-5,BNil))

    val in:BList[BList[Int]] = BCons(a1,BCons(a2,BNil))
    val result = BList.myFlatMap(in)
    BList.length(result) should be === (5)
    result should be === (BCons(2,BCons(4,BCons(6,BCons(-3,BCons(-5,BNil))))))
  }

  it should "do lfatMapping" in {
    val inp = BList(1,2,3)
    val result = BList.flatMap(inp)( i => BList(i,i))
    result should be (BList(1,1,2,2,3,3))
  }

}
