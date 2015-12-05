package fpscala.chapter7


import java.util.concurrent._


/**
 * Created by sajit on 12/2/15.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def isCancelled = false

    def get(timeout: Long, units: TimeUnit) = get

    def cancel(eventIfRunning: Boolean) = false

  }

}
