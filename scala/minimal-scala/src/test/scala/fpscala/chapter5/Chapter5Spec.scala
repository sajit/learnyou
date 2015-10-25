package fpscala.chapter5

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by sajit on 10/25/15.
 */
class Chapter5Spec extends FlatSpec with Matchers{

  it should "take 3 elements of a stream" in {
    val str = XStream(4,51,4,5,1)
    str.take(2).toList should be (List(4,51))
  }

}
