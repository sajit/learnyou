package fpscala.chapter6

import scala.util.Random


/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
object PreludeRNGs {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val tuple = rng.nextInt;
    tuple._1 match {
      case Int.MinValue => (Math.abs(tuple._1 + 1), tuple._2)
      case _ => (Math.abs(tuple._1), tuple._2)
    }
  }

  def double(rng:RNG): (Double,RNG) = {
    val tuple = nonNegativeInt(rng)
    tuple._1 match {
      case Int.MaxValue => (Int.MaxValue-1.toDouble/Int.MaxValue.toDouble,tuple._2)
      case i => (i.toDouble/Int.MaxValue.toDouble,tuple._2)
    }
  }

}
