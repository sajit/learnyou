package fpscala.chapter8

import fpscala.BaseSpec
class StateSpec extends BaseSpec{
  val simpleRng = RNG.Simple(10l)
  
  it should "map a state " in {  
    //convert a string to a state[string]
    val s1:State[RNG,String] = State.unit("hello")
    val s2: State[RNG,Int] = s1.map { x => x.length() }
    val (result,nextRNG):(Int,RNG) = s2.run(simpleRng)
    result should be (5)
    
    
  }
  
  it should "combine two states" in {
    val s1:State[RNG,String] = State.unit("hello")
    val s2: State[RNG,String] = State.unit("world") 
    val s3:State[RNG,String] = s1.map2(s2)((a,b) => a + b)
    val (result,nextRNG):(String,RNG) = s3.run(simpleRng)
    result should be ("helloworld")
  }
  
  it should "flatMap to learn" in {
    val s1:State[RNG,String] = State.unit("hello")
    val flatMapper:(String => State[RNG,String]) = { aString => State.unit(aString.toUpperCase()) } 
    val s2:State[RNG,String] = s1.flatMap(flatMapper)
    val (result,nextRNG):(String,RNG) = s2.run(simpleRng)
    result should be ("HELLO")
  }
  
}