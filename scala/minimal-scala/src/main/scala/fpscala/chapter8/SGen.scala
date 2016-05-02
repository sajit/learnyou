package fpscala.chapter8

case class SGen[A](g: Int => Gen[A]) {
  def choose2(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose2(start, stopExclusive).unsized

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(aNumber => g(aNumber).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    //val realFoo:(A => SGen[B]) = {actual => f(actual)}
    /**
     * a Function of Gen
     * aNum => Gen
     * g(aNumber) is Gen[A]
     * flatMap of Gen[A] where the function to flatMap is from A => Gen[B]
     * f(actual) is SGen[B]
     * f(actual).g(aNumber) is Gen[B]
     */
    SGen(aNumber => {
      g(aNumber).flatMap { actual => f(actual).g(aNumber) }
    })

  }
}