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

//  def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = f(z) match  {
//      case Some(a,t) => cons(a,unfold(t)(f))
//      case None => Stream.empty[A]
//
//  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => Stream.cons(h, unfold(s)(f))
      case None => Stream.empty
    }
}
