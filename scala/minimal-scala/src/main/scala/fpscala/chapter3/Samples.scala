package fpscala.chapter3

/**
 * Created by sajit on 5/14/15.
 */

sealed trait BList[+A]
case object BNil extends BList[Nothing]
case class BCons[+A](head:A,tail:BList[A]) extends BList[A]

object BList{
  def sum(ints:BList[Int]):Int = ints match {
    case BNil => 0
    case BCons(x,xs) => x + sum(xs)
  }

  def product(ints:BList[Int]):Int = ints match {
    case BNil => 1
    case BCons(x,xs) => x * product(xs)
  }

  def tail[A](l: BList[A]): BList[A] =
    l match {
      case BNil => sys.error("tail of empty list")
      case BCons(_,t) => t
    }
  def setHead[A](l: BList[A],newHead:A): BList[A] =
    l match {
      case BNil => BCons(newHead,BNil)
      case BCons(_,t) => BCons(newHead,t)
    }

  def drop[A](l: BList[A],n:Int): BList[A] =
    l match {
      case BNil => sys.error("drop on empty list")
      case BCons(h,t) => n match {
        case x if x<0 => sys.error("bad argument")
        case 0 => BCons(h,t)
        case x if x>0 => drop(t,n-1)
      }
    }

  def dropWhile[A](l: BList[A],predicate: A => Boolean): BList[A] =
    l match {
      case BNil => BNil
      case BCons(h,t) => {
        if(predicate(h)){
          BCons(h,dropWhile(t,predicate))
        }
        else{
          dropWhile(t,predicate)
        }
      }
    }

  def dropWhile2[A](l: BList[A],predicate: A => Boolean): BList[A] =
    l match {
      case BNil => BNil
      case BCons(h,t) => predicate(h) match {
        case true => BCons(h,dropWhile2(t,predicate))
        case false => dropWhile2(t,predicate)
      }
    }

  def append[A](a1:BList[A],a2:BList[A]):BList[A] = (a1,a2) match {
    case (BNil,a2) => a2
    case (BCons(h,t),a2) => BCons(h,append(t,a2))
  }


  def init[A](al:BList[A]):BList[A] = al match {
    case BNil => sys.error("impossible")
    case BCons(h,BNil) => BNil
    case BCons(h,t) => BCons(h,init(t))
  }

  def last[A](al:BList[A]):A = al match {
    case BNil => sys.error("nooo")
    case BCons(x,BNil) => x
    case BCons(h,t) => last(t)
  }

  def foldRight[A,B] (as:BList[A],z:B)(f:(A,B) => B):B = as match {
    case BNil => z
    case BCons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def length[A](as:BList[A]):Int = foldRight(as,0)((_,acc) => acc + 1)

  def foldLeft[A,B](as:BList[A],z:B)(f:(B,A) => B):B = as match {
    case BNil => z
    case BCons(x,xs) => foldLeft(xs,f(z,x))(f)
  }

  def reverse[A](aList:BList[A]):BList[A] = aList match {
    case BNil => BNil
    case BCons(x,xs) => append(reverse(xs),BCons(x,BNil))
  }

  //from solutions
  def revV2[A](a:BList[A]):BList[A] = foldLeft(a, BList[A]())((acc,h) => BCons(h,acc))

  def appendv2[A](a1:BList[A],a2:BList[A]):BList[A] = (a1,a2) match {
    case (BNil,a2) => a2
    case (BCons(h,t),a2) => BCons(h,foldRight(t,a2)((curr,acc) => appendv2(BCons(curr,BNil),acc)))
  }

  @annotation.tailrec
  def mfl[A,B](as:BList[A],z:B)(f:(B,A) => B): B = as match{
    case BNil => z
    case _ => {
      mfl(init(as),f(z,last(as)))(f)

    }
  }

  def sumIsh(as:BList[Int]):Int = mfl(as,0)((a,b) => a + b)

  def productIs(as:BList[Int]) = mfl(as,1)((a,b) => a * b)

  def map[A,B] (as:BList[A])(f: A => B):BList[B] = as match {
    case BNil => BNil
    case BCons(h,t) => BCons(f(h),map(t)(f))
  }

  def filter[A](as:BList[A],f:A => Boolean):BList[A] = as match {
    case BNil => BNil
    case BCons(h,t) => f(h) match {
      case true => BCons(h,filter(t,f))
      case false => filter(t,f)
    }
  }
    /**
   * Apply is a variadic function. A variadic function accepts zero or more arguments of that type.
   * In this example the type of argument A.
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as:A*):BList[A]= if(as.isEmpty) BNil else BCons(as.head,apply(as.tail: _*))
}
