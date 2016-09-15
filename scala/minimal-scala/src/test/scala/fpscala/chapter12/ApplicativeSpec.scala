package fpscala.chapter12

import fpscala.BaseSpec

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
}