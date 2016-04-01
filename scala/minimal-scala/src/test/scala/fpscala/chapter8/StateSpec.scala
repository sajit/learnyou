package fpscala.chapter8

import fpscala.BaseSpec
class StateSpec extends BaseSpec{
  it should "map a state " in {
    val simpleRng = RNG.Simple(10l)
    //val runner:(RNG => (String,RNG)) =  ??? 
    val s1:State[RNG,String] = State.unit("hello")
    val s2: State[RNG,Int] = s1.map { x => x.length() }
    val (result,nextRNG):(Int,RNG) = s2.run(simpleRng)
    result should be (5)
    
    
  }
  
}