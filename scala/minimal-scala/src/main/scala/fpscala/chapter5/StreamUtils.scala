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

}
