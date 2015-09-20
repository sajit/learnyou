package fpscala.chapter4

/**
 * 	Meaning	Scala notation
covariant	C[T’] is a subclass of C[T]	[+T]
contravariant	C[T] is a subclass of C[T’]	[-T]
invariant	C[T] and C[T’] are not related	[T]

 * Created by sajit on 9/19/15.
 */
sealed trait BOption[+A] {
  def map[B](f:A => B): BOption[B] = this match  {
    case BSome(x) => BSome(f(x))
    case BNone => BNone
  }

  def flatMap[B] (f: A => BOption[B]):BOption[B] = map(f) getOrElse BNone

  def getOrElse[B >:A](default: => B):B = this match {
    case BSome(x) => x
    case BNone => default
  }


}
case class BSome[+A](get:A) extends BOption[A]
case object BNone extends BOption[Nothing]

