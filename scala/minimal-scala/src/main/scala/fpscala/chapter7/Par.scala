package fpscala.chapter7

/**
 * Created by sajit.kunnumkal on 11/30/2015.
 */
class Par[+A] {
  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  def map2[A](a1: Par[A], a2: Par[A])(f: (A, A) => A): Par[A] = ???
}
