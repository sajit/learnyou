package fpscala.chapter7
import java.util.concurrent._

import scala.util._


object Par {

  type Par[A] = ExecutorService => Future[A]
  val globEs = Executors.newFixedThreadPool(1)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def mapWithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
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

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    
    val bFutures: List[Par[B]] = ps.map(asyncF(f))
    sequence(bFutures)
  }

  def parFilterNot[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    doParFilter(as)(f)(true)
  }

  private def doParFilter[A](as: List[A])(f: A => Boolean)(isNot: Boolean): Par[List[A]] = {
    val lpl: List[Par[List[A]]] = as map (asyncF(el => if (not(f, el, isNot)) List(el) else List()))
    map(sequence(lpl))(_.flatten)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  private def not[A](f: A => Boolean, a: A, isNot: Boolean) = if (isNot) {
    !f(a)
  } else {
    f(a)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = { a => lazyUnit(f(a)) }

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

  def sequencev2[A](ps:List[Par[A]]):Par[List[A]] = es => {
    val futures:List[Future[A]] = ps.map{ aPar => aPar(es)}
    val as:List[A] = futures.map { aFuture => aFuture.get() }
    UnitFuture(as)
    
  }
  
  def sequenceCopied[A](ps:List[Par[A]]):Par[List[A]] = ps match {
    case Nil => unit(List())
    case hPar :: tList => {
      map2(hPar,sequenceCopied(tList))((el,acc) => el :: acc)
    }
 
  }
  def parSum(as: List[Int]): Par[Int] = {
    if (as.length <= 1) {
      unit(as.headOption.getOrElse(0))
    }
    else {
      val (l, r): (List[Int], List[Int]) = as.splitAt(as.length / 2)
      val lPar: Par[Int] = parSum(l)
      val rPar: Par[Int] = parSum(r)
      map2(lPar, rPar)((e1, e2) => e1 + e2)
    }
  }

  def parSum1(as: List[Int]): Par[Int] = {
    if (as.length <= 1) {
      unit(as.headOption.getOrElse(0))
    }
    else {
      val (l, r): (List[Int], List[Int]) = as.splitAt(as.length / 2)
      val lPar: Par[Int] = parSum1(l)
      val rPar: Par[Int] = parSum1(r)
      map2(lPar, rPar)((e1, e2) => e1 + e2)
    }
  }

  /**
   * From github source
   * https://github.com/fpinscala/fpinscala/blob/master/answerkey/parallelism/06.answer.scala
   * @param as
   * @param f
   * @tparam A
   * @return
   */
  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    doParFilter(as)(f)(false)
  }
  
  def parFilterv3[A](as:List[A])(f: A => Boolean): Par[List[A]] = as match {
    case Nil => unit(List())
    case h :: t => {
      val parB:Par[Boolean] = asyncF(f)(h)
      map2(parB,parFilterv3(t)(f)){(el,acc) => if(el) {h :: acc} else {acc}}
    }
          
  }
  
  


  def parExists[A](as: List[A])(f: A => Boolean): Par[Boolean] = map(parFilter2(as)(f))(aList => !aList.isEmpty)

  def merge(a: Map[String, Int], b: Map[String, Int]): Map[String, Int] = {
    val merged: Map[String, Int] = a.map { case (x, y) => {
      val count = b.get(x).getOrElse(0) + y;
      (x, count)
    }
    }

    b ++ merged
  }
  
  def count(list:List[String]):Map[String,Int] = {
    list.groupBy { el => el }.mapValues { x => x.size }
  }

  def parWordCount(paras:List[List[String]],soFar:Map[String,Int]): Map[String,Int] = paras match {
    case Nil => soFar
    case h :: t => {
      val counts = count(h)
      val merged = merge(counts,soFar)
      parWordCount(t,merged)
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def isCancelled = false

    def get(timeout: Long, units: TimeUnit) = get

    def cancel(eventIfRunning: Boolean) = false

  }
}
