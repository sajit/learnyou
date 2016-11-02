package fpscala.chapter13

sealed trait Free[F[_],A] {
  def flatMap[B] (f: A => Free[F,B]):Free[F,B] = FRFlatMap(this,f) //TRFlatMap(this,f)
  def map[B](f: A => B):Free[F,B] = flatMap(f andThen( FRReturn(_)))
}
case class FRReturn[F[_],A] (a: A) extends Free[F,A]
case class FRSuspend[F[_],A](s:F[A]) extends Free[F,A]
case class FRFlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]