package fpscala.chapter2

/**
 * Created by sajit on 5/2/15.
 */
object Samples {

  def findFirst[A](arr:List[A],cond:(A => Boolean)):Int = {
    @annotation.tailrec
    def doFindFirst(list:List[A],currentIdx:Int):Int = {
      if(list.isEmpty){
        -1
      }
      else{
        val head = list.head
        if(cond(head)){
          currentIdx
        }
        else{
          doFindFirst(list.tail,currentIdx+1)
        }
      }
    }
    doFindFirst(arr,0)
  }

  def isSorted[A](as:List[A] ,ordered: (A,A) => Boolean):Boolean = {
    @annotation.tailrec
    def doIsSorted(list:List[A],prev:A):Boolean = list match {
      case Nil => true
      case (x::xs) => ordered(prev,x) && doIsSorted(xs,x)
  }

    doIsSorted(as,as.head)
  }

  def isSorted2[A](as:List[A],ordered:(A,A) => Boolean):Boolean = as match {
    case Nil => true
    case x::Nil => true
    case (x::y::xs) => ordered(x,y) && isSorted2(y::xs,ordered)
  }


}
