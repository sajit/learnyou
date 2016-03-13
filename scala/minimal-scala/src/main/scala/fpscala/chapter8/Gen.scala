package fpscala.chapter8





case class Gen[A](sample: State[RNG, A])
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
    def listOfN[A] (n:Int,g:Gen[A]):Gen[List[A]] = {
      
      Gen(State(rng => generateRandomList(n,rng)))
  }
  
    def generateRandomList[A](n:Int,rng:RNG):(List[A],RNG) = ???
}