package fpscala.chapter3

/**
 * Created by sajit on 9/18/15.
 */

sealed trait CTree[+A]
case class Leaf[A](value:A) extends CTree[A]
case class Branch[A](left:CTree[A],right:CTree[A]) extends CTree[A]

object CTree {
  def size[A](node:CTree[A]):Int = node match {
    case Leaf(x) => 1
    case Branch(left,right) => size(left) + size(right) + 1
  }

  def maximum(node :CTree[Int]):Int = node match {
    case Leaf(x) => x
    case Branch(left,right) => maximum(left) max maximum(right)
  }

  def depth[A] (node :CTree[A]):Int = node match {
    case Leaf(x) => 1
    case Branch(left,right) => (depth(left) max depth(right)) + 1
  }

  def map[A,B](node:CTree[A],f: A => B):CTree[B] = node match  {
    case Leaf(x) => Leaf(f(x))
    case Branch(left,right) => Branch(map(left,f),map(right,f))
  }

  def fold[A](node:CTree[A],f:(A,A) => A,g:A => A):A = node match {
    case Leaf(x) => g(x)
    case Branch(left,right) => f(fold(left,f,g),fold(right,f,g))
  }
}