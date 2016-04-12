package fpscala.chapter8

import fpscala.chapter5._




import Prop.TestCases
import Prop.Result
case class Prop(run: (TestCases,RNG) => Result)

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
    
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  
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
