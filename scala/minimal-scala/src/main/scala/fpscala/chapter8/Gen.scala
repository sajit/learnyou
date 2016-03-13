package fpscala.chapter8

case class Gen[A](sample: State[RNG, A]) {
  def choose2(start: Int, stopExclusive: Int): Gen[Int] = 
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))
}
object Gen {
 
    /* We could write this as an explicit state action, but this is far less
   convenient, since it requires us to manually thread the `RNG` through the
   computation. */
  def choose2(start: Int, stopExclusive: Int): Gen[Int] = 
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))
 
  def unit[A] (a: => A):Gen[A] = 
    Gen(State(rng => (a,rng)))
  
    def boolean:Gen[Boolean] = Gen(State(rng => (System.currentTimeMillis()%2==0,rng)))
    
    /**
     * From answerkey
     */
    def unit2[A](a: => A): Gen[A] = Gen(State.unit(a))
   
    /**
     * Generates lists of length n using the generator g 
     */
    def listOfN[A] (n:Int,g:Gen[A]):Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
  
    def listOfN2[A] (n:Int,g:Gen[A]):Gen[List[A]] = Gen(State.sequence(getStateList(n,g,List())))
    
    def getStateList[A](n:Int,g:Gen[A],soFar:List[State[RNG,A]]):List[State[RNG,A]] = {
    if(n<=0){
      soFar
    }
    else{
      val curr = g.sample
      getStateList(n-1,g,curr :: soFar)
    }
    
  }
  def longStringGen:State[RNG,String] = State(rng => ("Hello",rng))
  /**
   * Generate a list of strings of length 5 all of minLength
   */
  def largeStringsList(minLength:Int):Gen[List[String]] = Gen(State.sequence(List.fill(5)(longStringGen)))
}