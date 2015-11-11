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

}
