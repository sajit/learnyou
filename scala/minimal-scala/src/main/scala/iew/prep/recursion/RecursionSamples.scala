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
  
  def evaluateSeries(in:List[Int]):String = {
    
    if(in.length < 3) {
      "None"
    }
    else {
      if(in(2)-in(1)==in(1)-in(0)){
        //AP
        // for loop execution with a range
        var a = 1
      for(a <- 1 until in.size-2){
         if(in(a+2)-in(a+1) != in(a+1)-in(a)){
           return "None"
           }
           
        }
      "AP"
      }  
      else if(in(2)/in(1)==in(1)/in(0)) {
        //GP
        var a = 1
      for(a <- 1 until in.size-2){
         if(in(a+2)-in(a+1) != in(a+1)-in(a)){
           return "None"
           }
           
        }
      "GP"
        
      }
      else {
        "None"
      }
    }
  }
  /**
   * https://www.hackerrank.com/challenges/fibonacci-modified
   */
  def modifiedFibo(t1:Int,t2:Int,n:Int):Int = {
    var a=t1;
    var b=t2;
    var c = 0;
    for(i <- 1 to n){
      
      c = a + b*b;
      //System.out.println("a="+a+",b="+b+",c="+c);
      a = b;
      b = c;
    }
    return c;
  }
}