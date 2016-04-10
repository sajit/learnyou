package fpscala.chapter8

case class Gen[A](sample: State[RNG, A]) {
  def choose2(start: Int, stopExclusive: Int): Gen[Int] = 
      Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))
  
  /**
   * Sample is Type State[S,A]
   * sample.flatMap is { State [S,A] => State[S,B]}
   * f(a) = Gen[B]
   * f(a).sample = State [S,B]
   */
  def flatMap[B](f:A=>Gen[B]):Gen[B] = Gen(sample.flatMap(a => f(a).sample))
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
    if(n<=0){soFar}
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
  
  
  /**
   * A dumb function that returns  a generator of a list of length x (each element with value 5)
   */
  val listFlatMap:(Int => Gen[List[Int]]) = { x => Gen.unit2(List.fill(x)(5))}
    

  /**
   * Exercise 8.6-a
   */
  def listOfN3 (g:Gen[Int]):Gen[List[Int]] = g.flatMap(listFlatMap)
  
  def genericFlatMap[A](a:A):Gen[List[A]] =  Gen(State(rng => (List(a),rng)))
  /**
   * Exercise 8.6
   */
  def listOfN4[A](n:Int,g:Gen[A]):Gen[List[A]] = g.flatMap{ a => {
    if(n <=1) Gen.unit2(List(a))
    else{
      
      listOfN4(n-1,g).flatMap { aList => Gen.unit( aList.head :: aList) }
    }
  }}
  
  def intListOfN(n:Int,g:Gen[Int]):Gen[List[Int]] = g.flatMap{ a => {
    //println("A " + a)
    if(n <=1) Gen.unit2(List(a))
    else{
      
      intListOfN(n-1,Gen.unit2(a-1)).flatMap { aList => {
        //println("aList " + aList)
        Gen.unit( a:: aList) 
        }}
    }
  }}
  
  def union[A](g1:Gen[A],g2:Gen[A]):Gen[A] = {
    boolean.flatMap { b =>  
      if(b) g1 else g2
      }
   
  }
  
  val r = scala.util.Random
  def equiWeightedUnion[A](g1:Gen[A],g2:Gen[A]):Gen[A] = {
    if(r.nextFloat() < 0.5) g1 else g2
  }
 
  def weighted[A](g1:(Gen[A],Double),g2:(Gen[A],Double)):Gen[A] = {
    val (g11,d1) = g1
    val (g21,d2) = g2
    if(r.nextFloat() < d1.toFloat) g11 else g21
  }
}