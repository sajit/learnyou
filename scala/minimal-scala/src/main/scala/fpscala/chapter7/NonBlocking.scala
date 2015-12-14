package fpscala.chapter7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

/**
 * Created by sajit.kunnumkal on 12/14/2015.
 */
object NonBlocking {

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
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
