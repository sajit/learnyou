package iew.prep.recursion

object RecursionSamples {
  /**
   * Get xth fibonacci number
   */
  def fibo(x:Int):Int = {
    if(x==0 || x == 1){
      x
    }
    else {
      doFibo(0,1,x-2)
    }
  }
  def doFibo(lower:Int,higher:Int,remaining:Int):Int = {
    if(remaining == 0 ){
      higher
    }
    else{
      doFibo(higher,lower+higher,remaining-1)
    }
    
  }
}