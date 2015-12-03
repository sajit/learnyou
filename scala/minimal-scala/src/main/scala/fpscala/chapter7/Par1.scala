package fpscala.chapter7

/**
 * Created by sajit.kunnumkal on 11/30/2015.
 */

class Par1[+A] {
  def get[A](a: Par1[A]): A = ???

  def map2[A](a1: Par1[A], a2: Par1[A])(f: (A, A) => A): Par1[A] = ???

  def lazyUnit[A](a: => A): Par1[A] = fork(unit(a))

  def unit[A](a: A): Par1[A] = ???

  def fork[A](a: => Par1[A]) = ???
}
