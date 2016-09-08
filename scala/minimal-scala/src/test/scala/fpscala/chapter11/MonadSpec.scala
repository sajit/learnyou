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
  
}