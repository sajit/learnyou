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

  it should "take 4 values" in {
    val r = Process.take(4)(Stream(2,4,6,1,6,3))
    r.toList should be (List(2,4,6,1))
  }

  it should "drop 4 values " in {
    val r = Process.drop(4)(Stream(2,4,6,1,6,3))
    r.toList should be(List(6,3))
  }

  it should "takeWhile even " in {
    val f:(Int => Boolean) = (x => x%2 == 0)
    val r = Process.takeWhile(f)(Stream(2,4,6,1,6,3))
    r.toList should be(List(2,4,6))
  }

  it should "dropWhile even" in {
    val f:(Int => Boolean) = (x => x%2 == 0)
    val r = Process.dropWhile(f)(Stream(2,4,6,1,6,3))
    r.toList should be(List(1,6,3))
    val r2 = Process.dropWhile(f)(Stream(1,2,4,6,1,6,3))
    r2.toList should be (List(1,2,4,6,1,6,3))
  }

  it should "count " in {
    Process.count(Stream("a","b","c")) should be (Stream(1,2,3))
  }

}
