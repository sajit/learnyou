package fpscala.chapter7


import java.util.concurrent._

import scala.util._


/**
 * Created by sajit on 12/2/15.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  def map3[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)
    val timeout: Long = 1l
    val aValMb = Try(af.get(timeout, TimeUnit.SECONDS))
    val bValMb = Try(bf.get(timeout, TimeUnit.SECONDS))
    (aValMb, bValMb) match {
      case (Success(aVal), Success(bVal)) => UnitFuture(f(aVal, bVal))
      case _ => throw new Exception
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def isCancelled = false

    def get(timeout: Long, units: TimeUnit) = get

    def cancel(eventIfRunning: Boolean) = false

  }

}
