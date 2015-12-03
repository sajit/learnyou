package fpscala.chapter7

import java.util.concurrent.{ExecutorService, Future}

/**
 * Created by sajit on 12/2/15.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}
