import com.example.Foo
import org.scalatest._

import fpscala.chapter2._
class HelloSpec extends FlatSpec with Matchers {
  "Hello" should "have tests" in {
    true should be === true
  }

  "Chapter 2 exercises" should "work in" in {
    Chapter2.factorial(3) should be === 6
    Chapter2.tail_factorial(4) should be === 24

    Chapter2.fibo(4) should be === 5
    Chapter2.fibo(5) should be === 8



  }
  "Formatting " should "test" in {
    val expected = "The result of 4 is 24"
    Chapter2.formatResult(4,Chapter2.factorial) should be === expected
  }

  "Polymorphic parameterisms" should "test " in {
    val fooList = List(Foo("ponnu"),Foo("jose"))
    Chapter2.findFirst(fooList,Foo("jose")) should be ===1
  }
}
