package fpscala.chapter6

import scala.util.Random


/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
object PreludeRNGs {

  def nonNegativeInt(rng:Random):(Int,Random) = (Math.abs(rng.nextInt()),rng)

}
