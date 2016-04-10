package fpscala.chapter8




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
  
  def forAll[A](a:Gen[A])(f:A=> Boolean):Prop = ???

}
