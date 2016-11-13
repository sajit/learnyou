package fpscala.chapter13



sealed trait TailRec[A] {
  def flatMap[B] (f: A => TailRec[B]):TailRec[B] = TRFlatMap(this,f)
  def map[B](f: A => B):TailRec[B] = flatMap(f andThen( TRReturn(_)))
}

case class TRReturn[A](a: A) extends TailRec[A]
case class TRSuspend[A](resume: () => A) extends TailRec[A]


case class TRFlatMap[A,B](rec: TailRec[A], f: A => TailRec[B]) extends TailRec[B]