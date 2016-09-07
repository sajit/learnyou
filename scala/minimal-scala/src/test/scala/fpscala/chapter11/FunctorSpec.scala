package fpscala.chapter11

import fpscala.BaseSpec

class FunctorSpec extends BaseSpec {
  it should " maintain structure for identity function " in {
    val id: (Int => Int) = {a => a}
    val in = List(4,5,6,1,-1)
    val fList = FunctorUtils.listFunctor.map(in)(id)
    fList should be (in)
  }
  
  it should "distribute two lists" in {
    val evens = List(2,6,10)
    val odds = List(1,3,7)
    val fab = List((1,2),(3,6),(7,10))
    val (ll,rl) = FunctorUtils.listFunctor.distribute(fab)
    ll should be (odds)
    rl should be (evens)
  }
  
  it should "codistribute something " in {

    val e1 = Left(List(1,4))
    val result1 = FunctorUtils.listFunctor.codistribute(e1)
    result1 should be (List(Left(1),Left(4)))
    val e2 = Right(List("hello"))
    val result2 = FunctorUtils.listFunctor.codistribute(e2)
    result2 should be (List(Right("hello")))
  }
}