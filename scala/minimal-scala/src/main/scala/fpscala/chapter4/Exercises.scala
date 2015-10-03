package fpscala.chapter4

/**
 * Created by sajit on 9/24/15.
 */
object Exercises {

  def doVariance(doubles: List[Double], soFar: List[Double], mean : Double): List[Double] = doubles match {
    case Nil => soFar
    case x::t => doVariance(t, math.pow(x-mean,2)::soFar,mean)
  }

  def variance (xs:List[Double]):Option[Double] = xs match {
    case Nil => None
    case _ => Some(doMean(doVariance(xs,List(), doMean(xs))))
  }

  def doMean (x:Seq[Double]):Double = x.foldRight(0.0)((cur,soFar) => cur + soFar)/x.length

  def mean1(xs:Seq[Double]):Option[Double] = if(xs.isEmpty) None else Some(xs.sum /xs.length)

  def variance1(xs:Seq[Double]):Option[Double] = {
    val mean = mean1(xs)
    mean.flatMap{meanVal => {
      val squares = xs.map{el => math.pow(el-meanVal,2)}
      mean1(squares)
    }}.orElse(None)
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def map2[A,B,C] (a:Option[A],b:Option[B])(f:(A,B) => C): Option[C] = (a,b) match {
    case (None,None) => None
    case (None,_) => None
    case (_, None) => None
    case (Some(aVal),Some(bVal)) => Some(f(aVal,bVal))
  }

  def map2_real[A,B,C] (a:Option[A],b:Option[B])(f:(A,B) => C): Option[C] = a.flatMap{ aVal => b.flatMap{bVal => Some(f(aVal,bVal))}}

  var count = 0
  def sequence[A](a:List[Option[A]]):Option[List[A]] = {
    def doSequence(list:List[Option[A]],soFar:List[A]):Option[List[A]] = {
      if(list.isEmpty) Some(soFar)
      else {
        list.head.flatMap{ el =>  {
          count +=1
          doSequence(list.tail,el :: soFar)}}

      }
    }
    if(a.isEmpty) None
    else {
      count = 0
      doSequence(a,List())
    }
  }

  def resetCount() =  count = 0
  def getCount = count

  def traversev1[A,B](a:List[A])(f: A => Option[B]):Option[List[B]] = {
    sequence(a map f)
  }
  def traversev2[A,B](a:List[A])(f: A => Option[B]):Option[List[B]] =  {
    def doTraverse(al:List[A],soFar:List[B]):Option[List[B]] = {
        if(al.isEmpty) Some(soFar)
       else {
          f(al.head).flatMap{ el  =>  doTraverse(al.tail,el :: soFar)}
        }
    }
    doTraverse(a,List())
  }

  /**
   * From the source code
   * https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/errorhandling/Option.scala
   */
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traversev2(a)(x => x)


}

