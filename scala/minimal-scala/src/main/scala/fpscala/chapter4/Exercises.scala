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

}
