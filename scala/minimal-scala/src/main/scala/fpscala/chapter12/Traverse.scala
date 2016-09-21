package fpscala.chapter12

trait Traverse[F[_]] {
  def map[A,B](fa:F[A])(f:A=>B):F[B]
  def traverse[G[_]:Applicative,A,B](fa:F[A])(f: A => G[B]):G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fga:F[G[A]]):G[F[A]] = traverse(fga)(ga => ga)
}