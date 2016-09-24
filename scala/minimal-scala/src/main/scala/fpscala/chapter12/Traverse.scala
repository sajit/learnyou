package fpscala.chapter12

import fpscala.chapter11.Monad



trait Traverse[F[_]] {
  
   
  def map[A,B](fa:F[A])(f:A=>B):F[B] 

  def traverse[G[_]:Applicative,A,B](fa:F[A])(f: A => G[B]):G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fga:F[G[A]]):G[F[A]] = traverse(fga)(ga => ga)
}
case class Tree[+A](head: A, tail:List[Tree[A]])
object Traverse {
  val listTraverse = new Traverse[List] {
    def map[A,B](fa:List[A])(f: A=> B):List[B] = fa map f 
    
  }
  
  val optionTraverse = new Traverse[Option] {
    def map[A,B](oa:Option[A])(f: A => B):Option[B] = oa map f
  }
  
  
  
}