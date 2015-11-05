package fpscala.chapter5

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
}
