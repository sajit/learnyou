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


    Exercises.variance(List(4,4,4)) shouldEqual(Some(0.0))
  }

  it should "calculate variance 1" in {


    Exercises.variance1(List(4,4,4)) shouldEqual(Some(0.0))
  }

  it should "convert by lifting" in {
    Exercises.absO(Some("-2.0".toDouble)) shouldEqual(Some(2.0))

    Exercises.absO(None) shouldEqual(None)
  }

}
