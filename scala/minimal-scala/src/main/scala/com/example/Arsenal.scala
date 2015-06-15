package com.example


/**
 * Created by sajit on 6/14/15.
 */
object Arsenal {

  def addX(x:Int,el:List[Int]):List[Int] = el match {
    case Nil => Nil
    case (h::t) => (h+x) :: addX(x,t)
  }

}
