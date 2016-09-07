package fpscala.chapter11



trait Mon[F[_]] {
  def map[A,B](fa:F[A])(f: A => B):F[B] 
  def flatMap[A,B](fa:F[A])(f: A => F[B]):F[B] 
  def map2[A,B,C](fa:F[A],fb:F[B])(f:(A,B) => C):F[C]  = flatMap(fa)(a => map(fb)(b => f(a,b)))
}

object MonUtils {
  val listMon = new Mon[List] {
    def map[A,B](fa:List[A])(f:A => B):List[B] = fa map f
    def flatMap[A,B](fa:List[A])(f: A => List[B]):List[B]  = fa flatMap f
  }
  
  val optionMon = new Mon[Option] {
     def map[A,B](fa:Option[A])(f:A => B): Option[B] = fa map f
    def flatMap[A,B](fa:Option[A])(f: A => Option[B]):Option[B]  = fa flatMap f
  }
}