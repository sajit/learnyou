import com.example.Foo
import org.scalatest._

import fpscala.chapter2._
class HelloSpec extends FlatSpec with Matchers {


  "Chapter 2 exercises" should "work in" in {
    Chapter2.factorial(3) should be  (6)
    Chapter2.tail_factorial(4) should be (24)

    Chapter2.fibo(4) should be  (5)
    Chapter2.fibo(5) should be (8)



  }
  "Formatting " should "test" in {
    val expected = "The result of 4 is 24"
    Chapter2.formatResult(4,Chapter2.factorial) should be  (expected)
  }

  "Polymorphic parameterisms" should "test " in {
    val fooList = List(Foo("ponnu"),Foo("jose"))
    Chapter2.findFirst(fooList,Foo("jose")) should be (1)

    val intList = List(1,4,5,6,13)

    Samples.findFirst(intList,(x:Int) => (x%2==0)) should be  (1)


  }

  "Ordered function" should "be ordered " in {
    val in = List(2,3,5,6,12)

    Samples.isSorted(in,(x:Int,y:Int) => (x <= y)) should be (true)

    Samples.isSorted(List(3,4,1,10),(x:Int,y:Int) => (x<=y)) should be (false)

    Samples.isSorted2(in,(x:Int,y:Int) => (x <= y)) should be (true)

    Samples.isSorted2(List(3,4,1,10),(x:Int,y:Int) => (x<=y)) should be (false)

  }

  def zipWithIndex[I](z: List[I]):List[(I,Int)] = {
    def go(rem:List[I],acc:List[(I,Int)]):List[(I,Int)] = {
      if(rem.isEmpty){
        acc.reverse
      }
      else{
        go(rem.tail,(rem.head,acc.length+1) :: acc)
      }
    }
    go(z,List[(I,Int)]())
  }

  "zip with index" should "zipandcount" in {
    val z = List("hello","world","ds")
    zipWithIndex(z) should be (List(("hello",1),("world",2),("ds",3)))
  }
}
