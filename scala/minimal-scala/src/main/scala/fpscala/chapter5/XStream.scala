package fpscala.chapter5

/**
 * Created by sajit on 10/24/15.
 */
sealed trait XStream[+A] {
  /**
   * Exercise 5.1
   */
  def toList:List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  /**
   * Tail recursive version
   * @return
   */
  def toListTailRecursive:List[A] = {
    @annotation.tailrec
    def doToListTailRec(s:XStream[A], acc:List[A]):List[A] = s match {
      case Empty => acc
      case Cons(h,t) => doToListTailRec(t(),h() :: acc)
    }
    doToListTailRec(this,List()).reverse
  }

  /*
   Copied from github
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: XStream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }


}
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