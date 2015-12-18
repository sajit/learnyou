package fpscala.chapter8

import fpscala.chapter6.CandyShop.State
import fpscala.chapter6.RNG

/**
 * Created by sajit on 12/17/15.
 */
object PropTest {

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](A: Gen[A])(f: A => Boolean): Prop = ???

  trait Prop {
    def check: Unit

    def &&(p: Prop): Prop = ???
  }

  case class Gen[A](sample: State[RNG, A])

}
