package fpscala.chapter8

import fpscala.chapter5._
import fpscala.chapter8.Prop._

case class Prop(run: (TestCases,RNG) => Result){
  /**
   * copied from 
   * https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/testing/Gen.scala
   */
  def &&(p:Prop):Prop = Prop {
    (n,rng) => run(n,rng) match {
      case Passed => p.run(n,rng)
      case x => x
    }
  }
  
  def ||(p:Prop):Prop = Prop {
    (n,rng) => run(n,rng) match {
      case Passed => Passed
      case x => p.run(n,rng)
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  
  sealed trait Result {
    def isFalsified :Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure:FailedCase,successes:SuccessCount) extends Result {
    def isFalsified = true
  }
  
  def randomStream[A](g:Gen[A])(rng:RNG):Stream[A] = 
    StreamUtils. //Generates an infinite stream of A values by repeatedly sampling in generator
    unfold(rng)(rng => Some(g.sample.run(rng)))

  /**
   * Simply builds a string
   * @param s
   * @param e
   * @tparam A
   * @return
   */
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}"
  
  def forAll[A](as:Gen[A])(f:A=> Boolean):Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      //a stream of pairs (a,i) where a is a random value and i is its index in the stream
      case  (a,i)=> try{
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { 
        //if an exception happens record it
        case e:Exception => Falsified(buildMsg(a,e),i) }
      }.find(_.isFalsified).getOrElse(Passed)
    }  

}
