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
}