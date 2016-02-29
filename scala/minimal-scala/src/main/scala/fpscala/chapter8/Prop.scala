package fpscala.chapter8

trait Prop {
  
  def check:Boolean
  
  //copied from answer 3 chapter 8
    def &&(p: Prop): Prop = new Prop {
       def check = Prop.this.check && p.check
    }
}