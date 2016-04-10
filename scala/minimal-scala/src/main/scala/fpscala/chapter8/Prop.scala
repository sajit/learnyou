package fpscala.chapter8



trait Prop {
  
 
  def check:Boolean
  
  def betterCheck: Either[(Prop.FailedCase,Prop.SuccessCount),Prop.SuccessCount] = ???
  
  //copied from answer 3 chapter 8
    def &&(p: Prop): Prop = new Prop {
       def check = Prop.this.check && p.check
    }
}

object Prop {
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
