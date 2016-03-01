package fpscala.chapter8

import fpscala.chapter6.CandyShop.State
import fpscala.chapter6.RNG


case class Gen[A](sample: State[RNG, A])
object Gen {
  def choose(start:Int, stopExclusive:Int):Gen[Int] = ???
}