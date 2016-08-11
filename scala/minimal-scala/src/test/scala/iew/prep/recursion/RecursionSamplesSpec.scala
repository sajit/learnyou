package iew.prep.recursion

import fpscala.BaseSpec
class RecursionSamplesSpec extends BaseSpec{
  
  it should "find 5th fibonacci number" in {
    
    RecursionSamples.fibo(5) should be (3)
  }
  
  it should "identify an AP" in {
    RecursionSamples.evaluateSeries(List(2,4,6)) should be ("AP")
    
  }
  it should "identify the base cases" in {
     RecursionSamples.evaluateSeries(List(2)) should be ("None")
     
     RecursionSamples.evaluateSeries(List(2,4,5)) should be ("None")
  }
  
  it should "identify an GP" in {
    RecursionSamples.evaluateSeries(List(2,6,18)) should be ("GP")
    
  }
  
}