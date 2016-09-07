package fpscala.chapter11
import fpscala.chapter8.Gen

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A):F[A]
  def flatMap[A,B](ma:F[A])(f: A => F[B]):F[B]
  
  def map[A,B](ma:F[A])(f: A => B): F[B] = flatMap (ma)(a => unit(f(a)))
  
  def map2[A,B,C] (ma:F[A],mb:F[B])(f: (A,B) => C):F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))
  def map2_myv[A,B,C] (ma:F[A],mb:F[B])(f: (A,B) => C):F[C] = flatMap(ma)(a => flatMap(mb)(b => unit(f(a,b))))
}

object MonadUtils {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A):Gen[A]  = Gen.unit(a)
    def flatMap[A,B](ma:Gen[A])(f: A => Gen[B]):Gen[B] = ma flatMap f 
  }
}