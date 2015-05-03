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

}
