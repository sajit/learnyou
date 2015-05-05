package fpscala.chapter2

/**
 * Created by sajit on 5/4/15.
 */
object Partials {

  /**
   * partial1 is a function that takes A and a function f (that takes A,B and returns C)
   * and returns a function that takes B and returns C
   */
  def partial1[A,B,C](a:A,f:(A,B) =>C): B => C = (b:B) => f(a,b)

  /**
   * Curry is a function that takes a function f (that takes A , B and returns C)
   * and returns a function that takes A and returns a function from B => C
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def curry[A,B,C](f:(A,B) => C): A => (B => C) = (a:A) => partial1(a,f)

  //essentially the same as curry
  def curry1[A,B,C](f:(A,B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

  /**
   * Reverse transformation of curry
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def uncurry[A,B,C](f:A => B => C):(A,B)=> C = (a:A,b:B) => f(a)(b)

  //Correct :)
  def compose [A,B,C](f: B=> C, g:A => B): A => C = (a:A) => f(g(a))
}
