package fpscala.chapter12

import fpscala.BaseSpec
import java.text.SimpleDateFormat

class ApplicativeSpec extends BaseSpec{
  
  val applicative = Applicative.listApplicative
  it should "apply function to lists of same length" in {
    val a = List(1,3,7)
    val b = List(0,2,4)
    val result = applicative.map2(a,b)((x,y) => x*y)
    result should be (List(0,6,28))
  }
  it should "apply a function through map function " in {
    val fab:List[Int => Int]  = List((x => x + 1),(x => x%2)) 
    val fa = List(0,5)
    val result = applicative.apply(fab)(fa)
    result should be (List(1,1))
  }
  
  it should "validate a webform" in {
    val result = Applicative.validWebForm("hello", "2000-11-04", "1234567890")
    val date = new SimpleDateFormat("yyyy-MM-dd").parse("2000-11-04")
    result should be (Success(WebForm("hello", date, "1234567890")))
    Applicative.validWebForm("", "2004/11/44", "1333") match {
      case Failure(h,t) => t.size should be (2)
      case _ => fail()
    }
    
  }
  
  it should "applicative identiies.part1" in {
    Applicative.optionApplicative.map(Some(3))(x => x) should be (Some(3))
    
  }
  
  it should "applicative identity.part2" in {
    val g:(Int => Int) = {x => x + 1}
    val f:(Int => Int) = {x => x*2}
    val v = Some(4)
    val lhs = Applicative.optionApplicative.map(Applicative.optionApplicative.map(v)(g))(f)
    val rhs = Applicative.optionApplicative.map(v)(f compose g)
    lhs should be (rhs)
  }
}