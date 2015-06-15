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

  def append[A](a1:List[A],a2:List[A]):List[A] = (a1,a2) match {
    case (Nil,a2) => a2
    case (Cons(h,t),a2) => Cons(h,append(t,a2))
  }

  def init[A](al:List[A]):List[A] = al match {
    case Nil => sys.error("impossible")
    case Cons(h,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def last[A](al:List[A]):A = al match {
    case Nil => sys.error("nooo")
    case Cons(x,Nil) => x
    case Cons(h,t) => last(t)
  }

  def foldRight[A,B] (as:List[A],z:B)(f:(A,B) => B):B = as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def length[A](as:List[A]):Int = foldRight(as,0)((_,acc) => acc + 1)


  @annotation.tailrec
  def mfl[A,B](as:List[A],z:B)(f:(B,A) => B): B = as match{
    case Nil => z
    case _ => {
      mfl(init(as),f(z,last(as)))(f)

    }
  }

  def sumIsh(as:List[Int]):Int = mfl(as,0)((a,b) => a + b)

  def productIs(as:List[Int]) = mfl(as,1)((a,b) => a * b)

  def addX[T](x:Int,al : List[T]):List[T] = ???

    /**
   * Apply is a variadic function. A variadic function accepts zero or more arguments of that type.
   * In this example the type of argument A.
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as:A*):List[A]= if(as.isEmpty) Nil else Cons(as.head,apply(as.tail: _*))
}
