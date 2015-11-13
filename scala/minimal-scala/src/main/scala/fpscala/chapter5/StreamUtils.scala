package fpscala.chapter5

import scala.collection.immutable.Stream.cons


/**
 * Created by sajit.kunnumkal on 11/5/2015.
 */
object StreamUtils {
  /**
   * Exercise 5.8
   */
  def constant[A] (a:A):Stream[A] = {
    Stream.continually(a)
  }
//
  def from_1(n:Int):Stream[Int] = {
    val nStream = constant(n)
    nStream.zipWithIndex.map{case (el, index) => el + index}
  }

  def from_2(n:Int):List[Int] = {
    val ns = List.fill(10)(n)
    ns.zipWithIndex.map{case (el, indx) => el + indx}
  }

  def fibs(cur:Int,prev:Int):Stream[Int] = {
     Stream.cons(prev,fibs(cur+prev,cur))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => Stream.cons(h, unfold(s)(f))
      case None => Stream.empty
    }
//
  def fibsViaUnfold:Stream[Int] = unfold((0,1)){
    case (prev,curr) => Some(prev,(curr,prev+curr))
  }

  def fromViaUnfold(n:Int):Stream[Int] = unfold(n){el =>
    Some(el,el+1)
  }

  def constantViaUnfolds(x:Int):Stream[Int] = unfold(x)(el => Some(el,el))

  def mapViaUnfold[A,B](a:Stream[A],f: A => B): Stream[B] =
    unfold(a) {
      case Stream.cons(h,t) => Some((f(h), t))
      case _ => None
    }

  def takeViaUnfolds[A](a:Stream[A],n:Int):Stream[A] = unfold(a,n) {
    case (cons(h,t),1) => Some(h,(Stream.empty,0))
    case (cons(h,t),n) => Some(h,(t,n-1))
    case (empty,0) => None
    case (empty,n) => sys.error("how?")

  }

  def takeWhileViaUnfold[A](a:Stream[A],f: A => Boolean): Stream[A] = unfold(a) {
    case cons(h,t) => { if(f(h)) {
      Some((h,t))
    } else None}
    case empty => None
  }

  /**
   * copied from github answer key
   */
  def zipWith[A,B,C](s1:Stream[A],s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold(s1,s2) {

    case (cons(h1,t1),cons(h2,t2)) => Some(f(h1,h2),(t1,t2))
    case _ => None
  }

  /**
   * ZipAll function should continue traversal as long as either stream has more elements.
   * @param s1
   * @param s2
   * @tparam A
   * @tparam B
   * @return
   */
  def zipAll[A,B] (s1:Stream[A],s2:Stream[B]):Stream[(Option[A],Option[B])] = unfold(s1,s2) {

    //case (empty,empty) => None
    case (Stream.cons(h,t),empty) => Some((Some(h),None),(t,empty))
    case (empty, cons(h,t)) => Some((None,Some(h)),(empty,t))
    case (cons(h1,t1),cons(h2,t2)) => Some((Some(h1),Some(h2)),(t1,t2))

  }

  def zipAll2[A,B](s1:Stream[A],s2:Stream[B]):Stream[(Option[A],Option[B])] = (s1,s2) match {

    case (cons(h1,t1),cons(h2,t2)) => cons((Some(h1),Some(h2)),zipAll2(t1,t2))
    case (empty,cons(h,t)) => cons((None,Some(h)),zipAll2(empty,t))
    case (cons(h,t),empty) => cons((Some(h),None),zipAll2(t,empty))
    case _  => cons((None,None),Stream.empty)

  }
}
