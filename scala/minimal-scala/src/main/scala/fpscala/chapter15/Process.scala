package fpscala.chapter15



sealed trait Process[I,O]{

  def apply(s:Stream[I]):Stream[O] = this match {
    case Halt() => Stream()
    case Emit(h,t) => h #:: t(s)
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
  }
}

case class Emit[I,O](head:O,tail:Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](recv:Option[I] =>Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]

object  Process {
  def sum:Process[Double,Double] = {
    //Await takes in a function from option to Process
    def go(acc:Double):Process[Double,Double] = Await{
      case Some(d) => Emit(d + acc,go(d + acc))
      case None => Halt()
    }
    go(0.0)
  }

}