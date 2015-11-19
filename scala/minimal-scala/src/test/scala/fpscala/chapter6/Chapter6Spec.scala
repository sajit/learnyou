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


}
