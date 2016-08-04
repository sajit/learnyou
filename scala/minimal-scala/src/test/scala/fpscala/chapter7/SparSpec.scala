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
    val parC: Par[Int] = Par.mapWithTimeouts(aPar, bPar) { (x, y) => x + y}
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
      val parC: Par[Int] = Par.mapWithTimeouts(aPar, bPar) { (x, y) => x + y}
      Par.run(es)(parC).get()
    }
  }
  
  it should " sequenceify recursively " in {
    
    val es = Executors.newFixedThreadPool(1)
    val inList:List[Par[Int]] = List(Par.unit(4),Par.unit(2),Par.unit(5))
    val listPar:Par[List[Int]] = Par.sequenceCopied(inList)
    val resultList: List[Int] = Par.run(es)(listPar).get()
    resultList should be (List(4,2,5))
  }

  it should "calculate asynchronously " in {
    val f: (String => Int) = { x => Thread.sleep(2000)
      println("Slept for 2 secs")
      x.length
    }
    val bPar: Par[Int] = Par.asyncF(f)("hello")
    println("Immediately after async call")
    val es = Executors.newFixedThreadPool(2)
    bPar(es).get() should be(5)

  }

  it should "calculate asynchronously exists " in {
    val boolPar = Par.parExists(List(4, 5, 6, 7))(el => el % 5 == 0)
    val es = Executors.newFixedThreadPool(1)
    Par.run(es)(boolPar).get() should be(true)
  }


  it should "take some executor service and calculate sum " in {
    val es = Executors.newFixedThreadPool(3)
    val fSum = Par.parSum(List(3, 4, 5))
    fSum(es).get() should be(12)
  }
  
  it should "provide an alternative parallel filter " in {
    val es = Executors.newFixedThreadPool(3)
    val al = List(4,3,15,8,9,7)
    val filtered = Par.parFilterv3(al){x => x % 4 ==0 }
    Par.run(es)(filtered).get() should be (List(4,8))
    
  }
}
