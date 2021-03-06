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
    //fiboList.foreach{el => print(el+" ,")}
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

  it should "take elements of a string while matching a condition" in {
    val aStream = Stream("Hello", "World","is","awesome")
    val bigWords = StreamUtils.takeWhileViaUnfold[String](aStream,el => el.length > 4 )
    bigWords.toList should be (List("Hello","World"))
  }

  it should "add up two streams with zip " in {
    val odds = Stream(3,5,11)
    val evens = Stream(2,6,10)
    val sum = StreamUtils.zipWith(odds,evens)((e1,e2) => e1+e2)
    sum.toList should be (List(5,11,21))
  }

//  it should "zip streams " in {
//    val mbOdds = Stream(Some(3),None,Some(11))
//    val mbEvens = Stream(None,Some(6),Some(10))
//    val result = StreamUtils.zipAll(mbEvens,mbOdds)
//    result.toList should be (List((Some(3),None),(None,Some(6)),(Some(11),Some(10))))
//  }

  it should "zip streams without unfold  " in {
    val mbOdds = Stream(3)
    val mbEvens = Stream(6,10,4)
    val result = StreamUtils.zipAll2(mbOdds,mbEvens)
    result.toList should be (List((Some(3),Some(6)),(None,Some(10)),(None,Some(4)),(None,None)))
  }

  it should "check if one is a prefix of another" in {
    StreamUtils.startsWith(Stream(),Stream(4)) should be (true)
    StreamUtils.startsWith(Stream(4),Stream()) should be (false)

    StreamUtils.startsWith(Stream(4),Stream(4,5,6)) should be (true)

    StreamUtils.startsWith(Stream(5,5),Stream(5,6)) should be (false)
  }

  it should "create a stream of streams " in {
    val str = Stream(1,2,3)
    val result = StreamUtils.tails(str)
    result.toList.foreach{ el => println(el)}
    result.length should be (3)
  }

}


