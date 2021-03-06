package fpscala.chapter6

import fpscala.BaseSpec
import fpscala.chapter6.PreludeRNGs.Rand
import scala.util.Random

/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
class Chapter6Spec extends BaseSpec{

  it should "generate a non negative integer" in {
    val iterations:List[Int] = Range.inclusive(0,1000).toList
    val initRng:RNG = new SimpleRNG(Random.nextLong())
    iterations.foldRight(initRng)((_,el) =>
    {
      val result = PreludeRNGs.nonNegativeInt(el)
      result._1 should be >= (0)
      result._2
      }
    )

  }

  it should "not create duplicates" in {
    val rng1 = new SimpleRNG(1000)
    val (r1,rng2) = rng1.nextInt
    val (r11,rng3) = rng1.nextInt
    r1 should be (r11)
    r1 should not be (rng2.nextInt._1)
  }

  it should "min value doesnt have a non negative counterpart " in {
    Math.abs(Int.MinValue) should be < 0
  }

  it should "create a double between 0 and 1 " in {
    val rng:RNG = SimpleRNG(Random.nextLong())
    val (d,nrng) = PreludeRNGs.double(rng)
    val iterations:List[Int] = Range.inclusive(0,1000).toList
    iterations.foldRight(nrng)((_,el) => {
      val result = PreludeRNGs.double(el)
      result._1 should be >= (0.0)
      result._1 should be < (1.0)
      result._2
     }
    )

  }

  it should "a supposedly more elegant double " in {
    val rng:RNG = SimpleRNG(Random.nextLong())
    val rand1 = PreludeRNGs.moreElegantDouble
    val result:(Double,RNG) = rand1{rng}
    result._1 should be >= (0.0)
    result._1 should be < (1.0)



  }

  it should "generate a non-negative int less than" in {
    val rng:RNG = SimpleRNG(Random.nextLong())


    val iterations:List[Int] = Range.inclusive(0,1000).toList
    iterations.foldRight(rng)((_,el) => {
      val result = PreludeRNGs.nonNegativeLessThan(6)(rng)

      result._1 should be >= (0)
      result._1 should be < (6)
      result._2
    }
    )
  }
  
  it should "sequencify a list of Rands" in {
    val r1 = PreludeRNGs.unit(4)
    val r2 = PreludeRNGs.unit(3)
    val listOfRands = List(r1,r2)
    val randList:Rand[List[Int]] = PreludeRNGs.mySequence(listOfRands)
     val rng:RNG = SimpleRNG(Random.nextLong())
    val (result,rng2):(List[Int],RNG) = randList(rng)
    result.length should be (2)
    result should be (List(3,4))
  }
}
