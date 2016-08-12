package fpscala.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference
import java.lang.Exception


object NonBlocking {

  type NBPar[A] = ExecutorService => Future[A]

  def unit[A](a: A): NBPar[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def choice[A](condition: NBPar[Boolean])(a: NBPar[A], b: NBPar[A]): NBPar[A] = es => {
    val result: Boolean = run(es)(condition)
    if (result) {
      a(es)
    } else {
      b(es)
    }
  }
  
  def choiceN[A](n:NBPar[Int])(choices:List[NBPar[A]]):NBPar[A] = es => {
    val x = run(es)(n)
    x match {
      case a if a < 0 => throw new Exception()
      case a if a < choices.length => choices(a)(es)
      case a if a >= choices.length => throw new Exception()
    }
  }
  
  def choiceMap[K,V](key:NBPar[K])(choices:Map[K,NBPar[V]]):NBPar[V] = es => {
    val keyX = run(es)(key)
    choices.get(keyX).get (es)
    
  }

  def run[A](es: ExecutorService)(p: NBPar[A]): A = {
    val ref = new AtomicReference[A]() //A mutable thread-safe reference to use for storing the result
    val latch = new CountDownLatch(1) //allows threads to wait till countdown method is called x times
    p(es) { a => ref.set(a); ; latch.countDown() }
    latch.await()
    ref.get()

  }

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit

  }

}
