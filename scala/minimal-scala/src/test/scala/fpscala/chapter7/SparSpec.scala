package fpscala.chapter7

import java.util.concurrent._

import fpscala.BaseSpec
import fpscala.chapter7.Par.Par

/**
 * Created by sajit on 12/5/15.
 */
class SparSpec extends BaseSpec {


  it should "map some vals" in {
    val es = Executors.newFixedThreadPool(3)
    val aPar: Par[Int] = Par.unit(5)
    val bPar: Par[Int] = Par.unit(6)
    val parC: Par[Int] = Par.map3(aPar, bPar) { (x, y) => x + y}
    val result = Par.run(es)(parC).get()
    result should be(11)

  }

  it should "throw some Exception" in {
    val es = Executors.newFixedThreadPool(3)
    val aPar: Par[Int] = es => es.submit(new Callable[Int] {
      override def call(): Int = {
        Thread.sleep(3000)
        5
      }
    })
    val bPar: Par[Int] = Par.unit(6)
    intercept[Exception] {
      val parC: Par[Int] = Par.map3(aPar, bPar) { (x, y) => x + y}
      Par.run(es)(parC).get()
    }


  }

}
