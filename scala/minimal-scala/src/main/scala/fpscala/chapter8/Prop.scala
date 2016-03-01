package fpscala.chapter8

//object Prop{
//  type FailedCase = String
//  type SuccessCount = Int
//}
trait Prop {
  type FailedCase = String
  type SuccessCount = Int
 
  def check:Boolean
  
  def betterCheck: Either[(FailedCase,SuccessCount),SuccessCount] = ???
  
  //copied from answer 3 chapter 8
    def &&(p: Prop): Prop = new Prop {
       def check = Prop.this.check && p.check
    }
}