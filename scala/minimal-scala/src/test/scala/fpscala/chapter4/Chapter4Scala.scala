package fpscala.chapter4

import fpscala.chapter3.{BList, BNil, BCons}
import fpscala.chapter4
import org.scalactic.Tolerance
import org.scalatest.{Matchers, FlatSpec}

import fpscala.chapter4._

import Tolerance._
/**
 * Created
 * by sajit on 9/19/15.
 */
class Chapter4Scala extends FlatSpec with Matchers{

  it  should " meh " in {
    BSome(49).get should be (49)
  }

  it should "call map on an option without explicit type check " in {
    val maybe = BSome(40)

     maybe.map[Int](_.toInt).getOrElse(-1) should be (40)
     maybe.map(_.toInt).orElse(BSome(-1)) should be (BSome(40))

     val cruel:BOption[Int] = BNone
     cruel.map(_.toInt).getOrElse(-1) should be (-1)
     cruel.map(_.toInt).orElse(BNone) should be (BNone)
  }

  it should "calculate mean " in {
    Exercises.doMean(List(3,4,5)) should be (4)
  }

  it should "calculate variance " in {


    Exercises.variance(List(4,4,4)) shouldEqual Some(0.0)
  }

  it should "calculate variance 1" in {


    Exercises.variance1(List(4,4,4)) shouldEqual Some(0.0)
  }

  it should "convert by lifting" in {
    Exercises.absO(Some("-2.0".toDouble)) shouldEqual Some(2.0)

    Exercises.absO(None) shouldEqual None
  }

  it should "map two values together" in {
    val f:(Int,Int) => Int = (x,y) => x+y
    Exercises.map2_real(Some(5),None)(f) shouldEqual None
    Exercises.map2_real(None,None)(f) shouldEqual None
    Exercises.map2_real(None,Some(5))(f) shouldEqual None
    Exercises.map2_real(Some(5),Some(3))(f) shouldEqual Some(8)
   //
    Exercises.map2(Some(5),None)(f) shouldEqual None



  }

  it should "be None if any are None" in {
    val ll = List(Some(5),Some(2),None,Some(9))
    Exercises.sequence(ll) shouldEqual None
  }

  it should "be Some if only none are None" in {
    val ll = List(Some(5),Some(2),Some(1),Some(9))
    Exercises.sequence(ll) shouldEqual Some(List(9,1,2,5))
  }

  it should "only go far as first failure" in {
    val ll = List(Some(5),Some(2),None,Some(9))
    Exercises.resetCount()
    Exercises.getCount shouldEqual 0
    Exercises.sequence(ll)
    Exercises.getCount shouldEqual 2
  }

  def foo(x:String):Option[Int] = {
    try {
      Some(x.toInt)
    }
    catch {case e:Exception => None}
  }
  it should "traverse in v2 to None" in {
    val a = List("23","1a","34")
    Exercises.traversev2(a)(foo) shouldBe None
  }

  it should "traverse in v2 to Some" in {
    val a = List("23","11","34")
    Exercises.traversev2(a)(foo) shouldBe Some(List(34,11,23))
  }

}
