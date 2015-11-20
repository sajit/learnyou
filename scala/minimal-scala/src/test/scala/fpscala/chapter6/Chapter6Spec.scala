package fpscala.chapter6

import fpscala.BaseSpec
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
}
