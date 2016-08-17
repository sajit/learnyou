package fpscala.chapter10

trait Foldable[F[_]] {
  def foldRight[A,B](as:F[A])(z:B)(f:(A,B) => B):B
  def foldLeft[A,B](as:F[A])(z:B)(f:(B,A) => B):B
  def foldMap[A,B](as:F[A])(f: A => B)(mn:Monoid[B]):B
  def concatenate[A](as:F[A])(m:Monoid[A]):A = foldLeft(as)(m.zero)(m.op)
}