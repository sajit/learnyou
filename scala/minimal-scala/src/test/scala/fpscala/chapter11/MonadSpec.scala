package fpscala.chapter11

import fpscala.BaseSpec

class MonadSpec extends BaseSpec{
  

   it should "sequencify a list" in {
    val input = List(List(1),List(8))

    val result = MonadUtils.listMonad.sequence(input)
    
    result.size should be (1)
    result should be (List(List(1,8)))
  }
   it should "traverse a list" in {
    val input = List(1,8)
    val f:(Int => List[Boolean]) = (el => List(el%2==0))
     val result:List[List[Boolean]] = MonadUtils.listMonad.traverse(input)(f)

     result.size should be (1)
     result should be (List(List(false,true)))
 }
   
   it should "replciate 3 times " in {
     val result:List[List[Int]] = MonadUtils.listMonad.replicateM(3, List(1,4))
     result.size should be (2)
     result should be (List(List(1,1,1),List(4,4,4)))
   }
   
   it should "filter even monad lit " in {
     
    val result:List[List[Int]] =  MonadUtils.listMonad.filterM(List(1,3,2))((a => MonadUtils.listMonad.unit(a %2 ==0)))
    result should be (List(List(2)))
   }
   
   it should "be a klieisli composition " in {
     val listMonad = MonadUtils.listMonad;
     val f:(Int => List[Int]) = { x => List(x) }
     val g:(Int => List[Int] ) = {x => List.fill(2)(x) }
     val h:(Int => List[Int]) = { x => List() }
     val lhs = listMonad.compose(listMonad.compose(f, g), h)
     val rhs = listMonad.compose(f, listMonad.compose(g, h))
     lhs(0) should be (rhs(0))
   }
  
}