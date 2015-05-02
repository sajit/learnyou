package fpscala.chapter2

/**
 * Created by sajit on 5/1/15.
 */
object Chapter2 {

  //@annotation.tailrec
  /**
   * This is not a tailrec function as we are doing some computation. Hence compiler will throw an error
   * @param n
   * @return
   */
  def factorial(n:Int):Int = {
    n match {
      case 0 => 1
      case 1 => 1
      case _ => n * factorial(n-1)
    }
  }

  /**
   * tail_factorial is not a recursive call. So no sense in making tail rec
   * @param n
   * @return
   */
  def tail_factorial(n:Int):Int = {
    @annotation.tailrec
    def do_tail_factorial(n:Int,acc:Int):Int = {
      n match {
        case 0 => acc
        case 1 => acc
        case _ => do_tail_factorial(n-1,n*acc)
      }
    }
    do_tail_factorial(n,1)
  }

  def fibo(n:Int):Int = {
    @annotation.tailrec
    def do_fibo(n:Int,a:Int,b:Int):Int = {
      n match {
        case x if x<=1 => a + b
        case _ => do_fibo(n-1,b,a+b)
      }

    }
    do_fibo(n,0,1)
  }

  def formatAbs(x:Int) = {
    val message = "The absolute value of %d is %d"
    message.format(x, Math.abs(x))
  }

  def formatFactorial(x:Int) = {
    val message = "The factorial of %d is %d."
    message.format(x,factorial(x))
  }

  def formatResult(x:Int,f:Int=>Int):String = {
    val message = "The result of %d is %d"
    message.format(x,f(x))
  }
}
