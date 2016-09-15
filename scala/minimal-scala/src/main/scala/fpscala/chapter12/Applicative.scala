package fpscala.chapter12

import fpscala.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  //primitive combinators
  def map2[A,B,C](fa:F[A],fb:F[B])(f:(A,B) => C):F[C]
  def unit[A](a: => A):F[A]
  
  //derived combinators
  def map[A,B](fa:F[A])(f: A => B):F[B] = map2(fa,unit(()))((a,_) => f(a))
  
  def traverse[A,B](as:List[A])(f:A => F[B]):F[List[B]] = 
    as.foldRight(unit(List[B]()))((a,fbs) => map2(f(a),fbs)((a,bs) => a :: bs))
    
  def sequence[A](fas:List[F[A]]):F[List[A]] = fas.foldRight(unit(List[A]()))((fa,acc) => map2(fa,acc)((a,alist) => a :: alist))
  def replicateM[A](n:Int,fa:F[A]):F[List[A]] = sequence(List.fill(n)(fa))
   
  def product[A,B](fa:F[A],fb:F[B]):F[(A,B)] = map2(fa,fb)((a,b) => (a,b))
  
  /**
   * apply in terms of map2
   */
  def apply[A,B](fab:F[A => B])(fa:F[A]):F[B] = map2(fab,fa)((f,a) => f(a))
 
  
  /** map in terms of apply */
  def mapv2[A,B](fa:F[A])(f: A => B):F[B] = apply(unit(f))(fa)
  
  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2v3[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
    apply(map(fa)(f.curried))(fb)
}

object Applicative {
  
  val listApplicative = new Applicative[List] {
    def unit[A](a: => A):List[A] = List(a)
    def map2[A,B,C](fa:List[A],fb:List[B])(f:(A,B) => C):List[C] = fa zip fb map (tuple => f(tuple._1,tuple._2))
  }
}