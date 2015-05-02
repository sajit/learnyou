import fpscala.chapter2.Chapter2
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class HelloSpec extends FlatSpec with ShouldMatchers {
  "Hello" should "have tests" in {
    true should be === true
  }

  "Chapter 2 exercises" should "work in" in {
    Chapter2.factorial(3) should be === 6
    Chapter2.tail_factorial(4) should be === 24

    Chapter2.fibo(4) should be === 5
    Chapter2.fibo(5) should be === 8

  }
}
