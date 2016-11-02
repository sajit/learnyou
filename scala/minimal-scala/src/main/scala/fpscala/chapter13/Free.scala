package fpscala.chapter13

sealed trait Free[F[_],A]
case class Return[F[_],A] (a: A) extends Free[F,A]
case class Suspend[F[_],A](s:F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]