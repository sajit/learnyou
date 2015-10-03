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

  def orElse[EE >:E, B >:A] (b: => Beither[EE,B]):Beither[EE,B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >:E,B,C](b:Beither[EE,B])(f:(A,B) => C):Beither[EE,C] = (this,b) match {
    case (_,Left(e)) => Left(e)

    case (Left(e),_) => Left(e)
    case (Left(e1),Left(e2)) => Left(e1)
    case (Right(a1),Right(a2)) => Right(f(a1,a2))
  }

  //Alternative version of map2
  def map2v2[EE >: E, B, C](b: Beither[EE, B])(f: (A, B) => C):
  Beither[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)

}
case class Left[+E](value:E) extends Beither[E, Nothing]
case class Right[+A](value:A) extends Beither[Nothing,A]

object Beither {
  def sequence[E,A](a:List[Beither[E,A]]):Beither[E,List[A]] = {
    def doSequence(list:List[Beither[E,A]],soFar:List[A]):Beither[E,List[A]] = {
      if(list.isEmpty) {
        Right(soFar)
      }
      else{
        list.head match {
          case Left(x) => Left(x)
          case Right(x) => doSequence(list.tail,x :: soFar)
        }
      }
    }
    doSequence(a,List())
  }


  def traverse[E,A,B](es: List[A])(f: A => Beither[E, B]): Beither[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
}

