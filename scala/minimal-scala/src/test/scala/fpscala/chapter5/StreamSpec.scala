package fpscala.chapter5

import org.scalatest.{Matchers, FlatSpec}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global


/**
 * Created by sajit.kunnumkal on 11/5/2015.
 */
class StreamSpec extends FlatSpec with Matchers{

  it should "create an infinite stream " in {
    val ones:Stream[Int] =  Stream.continually(1)

    lazy val f:Future[Boolean] = Future {
      ones.exists(el => el %2 == 0)
    }
    intercept[TimeoutException] { Await.result(f, 1 second) }

  }

  it should "create a list of 100 elements with increasing values " in {
    val result:List[Int] = StreamUtils.from_2(1)
    result.length should be (10)
    result should be (sorted)
  }

  it should "create a stream of length 100 elements with increasing values " in {
    val str:Stream[Int] = StreamUtils.from_1(1)
    lazy val f = Future {
      str should be (sorted)
    }
    intercept[TimeoutException] { Await.result(f, 1 second) }

  }

  it should "create a fibonacci stream " in {
    val fibo = StreamUtils.fibs(1,0)
    val fiboList = fibo.take(10).toList
    fiboList.foreach{el => print(el+" ,")}
  }

  it should "create a from via unfold " in {
    val from = StreamUtils.fromViaUnfold(2).take(5).toList
    from should be (List(2,3,4,5,6))
  }

  it should "calculate fibonacci by unfolding" in {
    val fibo = StreamUtils.fibsViaUnfold.take(6).toList
    fibo should be (List(0,1,1,2,3,5))
  }

  it should "create constant via unfold " in {
    val from = StreamUtils.constantViaUnfolds(2).take(5).toList
    from should be (List(2,2,2,2,2))
  }

  it should "map a stream via unfold" in {
    val aStream = Stream("Hello", "World","is","awesome")
    val lengthStream = StreamUtils.mapViaUnfold[String,Int](aStream, el => el.length)
    lengthStream.toList should be (List(5,5,2,7))
  }

  it should "take n elements of a stream via unfold " in {
    val aStream = Stream("Hello", "World","is","awesome")
    val shortStream = StreamUtils.takeViaUnfolds(aStream,2)
    shortStream.toList should be (List("Hello","World"))
  }
}
