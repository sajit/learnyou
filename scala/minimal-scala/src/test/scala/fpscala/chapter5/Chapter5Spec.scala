package fpscala.chapter5

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by sajit on 10/25/15.
 */
class Chapter5Spec extends FlatSpec with Matchers{

  it should "take 2 elements of a stream" in {
    val str = XStream(4,51,4,5,1)
    str.take(2).toList should be (List(4,51))
  }

  it should "drop 3 elements of a stream" in {
    val str = XStream(4,5,3,5,1,50)
    str.drop(3).toList should be (List(5,1,50))
  }

  it should "take elements till no match " in {
    val str = XStream(2,6,1,4,10)
    str.takeWhile(el => el % 2 == 0).toList should be (List(2,6))
    str.takeWhile(el => el % 2 != 0).toList should be (Nil)
  }

  it should "detect existence " in {
    val str = XStream(3, 5, 10, 2, 23)
    str.exists2(x => x > 25) should be(false)
    str.exists2(x => (x % 10 == 0)) should be(true)
  }

  it should "return true iff all meet " in {
    val result = XStream(3, 5, 7, 11).forAll(el => el % 2 != 0)
    result should be(true)
    XStream(3, 4, 5, 7).forAll(el => el % 2 != 0) should be(false)
  }

  it should "take while as fold right " in {
    val str = XStream(2, 6, 1, 4, 10)
    str.takeWhile2(el => el % 2 == 0).toList should be(List(2, 6))
    str.takeWhile2(el => el % 2 != 0).toList should be(Nil)
  }

  it should "map a stream using right folding " in {
    val str = XStream(3,4,5,6,1)
    val mappedStream = str.map_1(x => 2*x)
    mappedStream.toList should be (List(6,8,10,12,2))
  }

}
