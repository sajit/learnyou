package fpscala.chapter6

import fpscala.BaseSpec
import scala.util.Random

/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
class Chapter6Spec extends BaseSpec{

  it should "generate a non negative integer" in {
    val result = PreludeRNGs.nonNegativeInt(new Random())
    result._1 should be > (0)
  }

  it should "not create duplicates" in {
    val rng1 = new SimpleRNG(1000)
    val (r1,rng2) = rng1.nextInt
    val (r11,rng3) = rng1.nextInt
    r1 should be (r11)
    r1 should not be (rng2.nextInt._1)
  }

}
