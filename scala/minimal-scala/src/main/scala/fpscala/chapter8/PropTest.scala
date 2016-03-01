package fpscala.chapter8


/**
 * Created by sajit on 12/17/15.
 */
object PropTest {

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](A: Gen[A])(f: A => Boolean): Prop = ???

  
  
  

}
