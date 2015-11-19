package fpscala.chapter6

import scala.util.Random

/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
case class SimpleRNG(seed:Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val rng = new Random(seed)
    val nextSeed = rng.nextLong()
    (rng.nextInt(),new SimpleRNG(nextSeed))
  }
}
