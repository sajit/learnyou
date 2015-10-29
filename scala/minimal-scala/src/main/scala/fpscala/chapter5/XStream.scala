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

  def take(n:Int):XStream[A] = this match {
    case Cons(h,t) => {
      if(n > 1) {

        XStream.cons(h(),t().take(n-1))
      }
      else if(n == 1) {
        XStream.cons(h(),Empty)
      }
      else {
        this
      }
    }
    case _ => sys.error("not_possible")
  }

  def drop(n:Int):XStream[A] = this match {
    case Cons(h,t) => {
      if(n > 0){
        t().drop(n-1)
      }
      else if(n ==0){
        this
      }
      else {
        sys.error("error in judgement")
      }
    }
    case _ => sys.error("not_possible")
  }

  def takeWhile(p: A => Boolean):XStream[A] = this match {
    case Cons(h,t) => {
      lazy val head = h
      if(p(head())){
        XStream.cons(head(),t().takeWhile(p))
      }
      else{
        Empty
      }
    }
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((x, y) => p(x) || y)


  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  //Not sure why the following is not working
  def takeWhile2(p: A => Boolean): XStream[A] = foldRight(XStream.empty[A])((a, b) => {
    if (p(a)) {
      XStream.cons(a, b.takeWhile2(p))
    } else {
      XStream.empty
    }
  })

  //From github source code
  def takeWhile_1(p: A => Boolean): XStream[A] = foldRight(XStream.empty[A])((h, t) => {
    if (p(h)) XStream.cons(h, t)
    else
      XStream.empty
  })

  //dumb implementation
  def map[B](f: A => B):XStream[B] = this match {
    case Empty => Empty
    case Cons(h,t) => XStream.cons(f(h()),t().map(f))
  }

  def map_1[B](f: A=> B):XStream[B] = foldRight(XStream.empty[B])((h,t) => {
    XStream.cons(f(h),t)
  })

  def filter(p: A => Boolean):XStream[A] = foldRight(XStream.empty[A])((h,t) => {
    if(p(h)){
      XStream.cons(h,t)
    }else{
      t
    }
  })

  def append[B>:A](b:XStream[B]):XStream[B] =  foldRight(b)((h,t) => {
    XStream.cons(h,t)
  })

  def flatMap[B](f:A =>XStream[B]):XStream[B] = foldRight(XStream.empty[B])((h,t) => {
    t.append(f(h))

  })

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