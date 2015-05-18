package fpscala.chapter3

/**
 * Created by sajit on 5/14/15.
 */
object Samples {

}
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List{
  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ints:List[Int]):Int = ints match {
    case Nil => 1
    case Cons(x,xs) => x * product(xs)
  }

  def tail(xs:List) = xs match {
    case Nil => throw Exception
    case Cons(_,xs) => xs
  }
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }
  def setHead[A](l: List[A],newHead:A): List[A] =
    l match {
      case Nil => Cons(newHead,Nil)
      case Cons(_,t) => Cons(newHead,t)
    }

  def drop[A](l: List[A],n:Int): List[A] =
    l match {
      case Nil => sys.error("drop on empty list")
      case Cons(h,t) => n match {
        case x if x<0 => sys.error("bad argument")
        case 0 => Cons(h,t)
        case x if x>0 => drop(t,n-1)
      }
    }

  def dropWhile[A](l: List[A],predicate: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h,t) => {
        if(predicate(h)){
          Cons(h,dropWhile(t,predicate))
        }
        else{
          dropWhile(t,predicate)
        }
      }
    }

  def dropWhile2[A](l: List[A],predicate: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h,t) => predicate(h) match {
        case true => Cons(h,dropWhile2(t,predicate))
        case false => dropWhile2(t,predicate)
      }
    }
  /**
   * Apply is a variadic function. A variadic function accepts zero or more arguments of that type.
   * In this example the type of argument A.
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as:A*):List[A]= if(as.isEmpty) Nil else Cons(as.head,apply(as.tail: _*))
}
