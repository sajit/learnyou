package fpscala.chapter7

import fpscala.BaseSpec
import java.util.concurrent._
import NonBlocking._

class NonBlockingSpec extends BaseSpec{
  
  val es = Executors.newFixedThreadPool(3)
  
  it should "apply a future" in {
    
    val callable = new Callable[Int] {
      def call = 5
      
    }
    val future5 = new FutureTask(callable);
    val nbPar = {execS:ExecutorService => future5}
    //val result = NonBlocking.run(es)(nbPar)
    //result should be(5)
  }
  
  it should "choiceN " in {
    val fList:List[NBPar[Int]] = List(unit(3),unit(2),unit(1))
    val result = NonBlocking.run(es)(choiceN(NonBlocking.unit(0))(fList))
    result should be (3)
  }
  
  it should "map choices " in {
    val fMap:Map[Int,NBPar[Int]] = Map(0 -> unit(3),1 -> unit(2),2 -> unit(1))
    val result = NonBlocking.run(es)(choiceMap(NonBlocking.unit(0))(fMap))
    result should be (3)
  }
  
}