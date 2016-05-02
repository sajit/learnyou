package fpscala.chapter8

case class SGen[A](forSize: Int => Gen[A]) {
  def choose2(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose2(start, stopExclusive).unsized

  def apply(n: Int): Gen[A] = forSize(n)
}