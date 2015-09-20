package fpscala.chapter4

/**
 * 	Meaning	Scala notation
covariant	C[T’] is a subclass of C[T]	[+T]
contravariant	C[T] is a subclass of C[T’]	[-T]
invariant	C[T] and C[T’] are not related	[T]

 * Created by sajit on 9/19/15.
 */
sealed trait BOption[+A] {
  def map[B](f:A => B): Option[B] = ???
}
case class BSome[+A](get:A) extends BOption[A]
case object BNone extends BOption[Nothing]
