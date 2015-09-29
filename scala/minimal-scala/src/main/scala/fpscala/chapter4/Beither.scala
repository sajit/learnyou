package fpscala.chapter4

/**
 * Created by sajit on 9/28/15.
 */

sealed trait Beither[+E,+A]
case class Left[+E](value:E) extends Beither[E, Nothing]
case class Right[+A](value:A) extends Beither[Nothing,A]