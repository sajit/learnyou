package fpscala.chapter7

/**
 * Created by sajit.kunnumkal on 12/14/2015.
 */
object NonBlocking {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit

  }


}
