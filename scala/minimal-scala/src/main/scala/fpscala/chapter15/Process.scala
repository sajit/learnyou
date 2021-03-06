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

  /**
    * compose..
    * copied from https://github.com/fpinscala/fpinscala/blob/master/answerkey/streamingio/05.answer.scala
    * @param p2
    * @tparam O2
    * @return
    */
  def |>[O2](p2:Process[O,O2]):Process[I,O2] = {
    p2 match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(f) => this match {
        case Emit(h, t) => t |> f(Some(h))
        case Halt() => Halt() |> f(None)
        case Await(g) =>  Await((i: Option[I]) => g(i) |> p2)
      }
    }
  }

}

case class Emit[I,O](head:O,tail:Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](recv:Option[I] =>Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]

object  Process {
  def takeWhile[I](predicate:I => Boolean):Process[I,I] = {
    def go():Process[I,I] = Await{
      case Some(i) => {
        if(predicate(i)){
          Emit(i,go())
        }
        else {
          Halt()
        }
      }
      case None => Halt()
    }
    go()
  }

  def dropWhile[I](predicate:I => Boolean):Process[I,I] = {
    def go(hasFailed:Boolean):Process[I,I] = Await{
      case Some(i) => {
        if(hasFailed){
          Emit(i,go(hasFailed))
        }
        else{
          if(!predicate(i)){
            Emit(i,go(true))
          }
          else{
            go(false)
          }

        }

      }
      case None => Halt()
    }
    go(false)
  }

  def sum:Process[Double,Double] = {
    //Await takes in a function from option to Process
    def go(acc:Double):Process[Double,Double] = Await{
      case Some(d) => Emit(d + acc,go(d + acc))
      case None => Halt()
    }
    go(0.0)
  }

  def take[I](n:Int):Process[I,I] = {
    def go(x:Int):Process[I,I] = x match {
      case 0 => Halt()
      case l:Int => Await {
        case Some(i) => Emit(i,go(l-1))
        case None => Halt()
      }
    }
    go(n)
  }

  def drop[I](n:Int):Process[I,I] = {
    def go(x:Int):Process[I,I] = {

        Await {
          case Some(i) => {
              if(x>0){
                  go(x-1)
              }else {
                Emit(i, go(0))
              }
          }
          case None => Halt()
        }

    }

    go(n)
  }

  def count[I]:Process[I,Int] = {
    def go(count:Int):Process[I,Int] = Await {
      case Some(i) => Emit(count,go(count + 1))
      case None => Halt()
    }
    go(1)
  }

  def mean:Process[Double,Double] = {
    def go(sum:Double,n:Int):Process[Double,Double] = Await {
      case Some(d) => Emit((d + sum)/(n + 1),go(d + sum,n + 1))
      case None => Halt()
    }
    go(0.0,0)
  }

  def exists[I](f:I => Boolean):Process[I,Boolean] =  {
    def go():Process[I,Boolean] = Await {
      case Some(i) => if(f(i)){ Emit(true,Halt())} else { go()}
      case None => Emit(false,Halt())
    }
    go()
  }

}