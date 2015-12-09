package fpscala.chapter7


import java.util.concurrent._

import scala.util._


/**
 * Created by sajit on 12/2/15.
 */

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

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

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val bFutures: List[Par[B]] = ps.map(asyncF(f))
    sequence(bFutures)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = { a => lazyUnit(f(a)) }

  def parFilter1[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val filtered: List[Par[A]] = as.map { el => if (f(el)) lazyUnit(List(el)) else lazyUnit(List())}.flatten
    sequence(filtered)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  })

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
   * folds over a list of Pars. and does something.. meh.. compiles atleast.
   * @param ps
   * @tparam A
   * @return
   */

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((aParElement, parOfAList) =>
    map2(aParElement, parOfAList)((element, acc) => element :: acc))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def isCancelled = false

    def get(timeout: Long, units: TimeUnit) = get

    def cancel(eventIfRunning: Boolean) = false

  }

}
