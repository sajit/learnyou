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
}
