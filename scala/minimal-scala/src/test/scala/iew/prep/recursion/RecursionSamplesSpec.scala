package iew.prep.recursion

import fpscala.BaseSpec
class RecursionSamplesSpec extends BaseSpec{
  
  it should "find 5th fibonacci number" in {
    
    RecursionSamples.fibo(5) should be (3)
  }
  
}