package fpscala.chapter5

/**
 * Created by sajit on 10/24/15.
 */
sealed trait XStream[+A]
case object Empty extends XStream[Nothing]
case class Cons[+A](h: () => A, t: () => XStream[A]) extends XStream[A]
object XStream {

  def cons[A](hd: => A, tl: => XStream[A]):XStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head,() => tail)
  }

  def empty[A]:XStream[A] = Empty

  def apply[A] (as : A*):XStream[A] =
  if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption[A](xStream: XStream[A]):Option[A] = xStream match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

}