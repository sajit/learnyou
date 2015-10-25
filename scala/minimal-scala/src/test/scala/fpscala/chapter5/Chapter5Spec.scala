package fpscala.chapter5

import org.scalatest.{Matchers, FlatSpec}

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

}
