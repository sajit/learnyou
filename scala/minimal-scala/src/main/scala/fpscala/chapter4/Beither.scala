package fpscala.chapter4

/**
 * Created by sajit on 9/28/15.
 */

sealed trait Beither[+E,+A] {
  //from the solutions :(
  //https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/errorhandling/Either.scala
  def map[B](f: A => B): Beither[E,B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >:E, B] (f: A => Beither[EE,B]):Beither[EE,B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
}
case class Left[+E](value:E) extends Beither[E, Nothing]
case class Right[+A](value:A) extends Beither[Nothing,A]

