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
  
  def map2v2[A,B,C](fa:F[A],fb:F[B])(f:(A,B) => C):F[C] = ???
  
  def mapv2[A,B](fa:F[A])(f: A => B):F[B] = apply(unit(f))(fa)
}