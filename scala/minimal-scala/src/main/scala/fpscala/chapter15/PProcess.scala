package fpscala.chapter15

trait PProcess[F[_],O]

object PProcess {

  /**
    * @param req
    * @param recv function now takes an Either so we can handle errors
    * @tparam F
    * @tparam A
    * @tparam O
    */
  case class Await[F[_],A,O](
                            req:F[A],recv:Either[Throwable,A] => Process[F[A],O]
                            ) extends PProcess[F,O]

  case class Emit[F[_],O](head:O,tail:PProcess[F,O]) extends PProcess[F,O]

  case class Halt[F[_],O](err:Throwable) extends PProcess[F,O]

  case object End extends Exception

  case object Kill extends Exception
}
