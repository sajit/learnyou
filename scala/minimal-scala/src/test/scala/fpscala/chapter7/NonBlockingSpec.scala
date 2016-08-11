package fpscala.chapter7

import fpscala.BaseSpec
import java.util.concurrent._

class NonBlockingSpec extends BaseSpec{
  
  it should "apply a future" in {
    val es = Executors.newFixedThreadPool(3)
    val callable = new Callable[Int] {
      def call = 5
      
    }
    val future5 = new FutureTask(callable);
    val nbPar = {execS:ExecutorService => future5}
    //val result = NonBlocking.run(es)(nbPar)
    //result should be(5)
  }
  
}