package fpscala.chapter11
import fpscala.chapter8.Gen
import fpscala.chapter7.Par

case class Id[A](value:A){
  def map[B](f: (A => B)): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A):F[A]
  def flatMap[A,B](ma:F[A])(f: A => F[B]):F[B]
  
  def map[A,B](ma:F[A])(f: A => B): F[B] = flatMap (ma)(a => unit(f(a)))
  
  def map2[A,B,C] (ma:F[A],mb:F[B])(f: (A,B) => C):F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))
  def map2_myv[A,B,C] (ma:F[A],mb:F[B])(f: (A,B) => C):F[C] = flatMap(ma)(a => flatMap(mb)(b => unit(f(a,b))))
  
  def sequence[A](lma:List[F[A]]):F[List[A]] =  lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  
  def traverse[A,B](la:List[A])(f: A => F[B]):F[List[B]] = sequence(la.map { el => f(el) })
  def traverse_correct[A,B](la:List[A])(f: A => F[B]):F[List[B]] = la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)((b,lb) => b :: lb))
  
  /**
   * For `List`, the `replicateM` function will generate a list of lists. It will contain all the lists of length `n` with 
   * elements selected from the input list.
   * 
   * For `Option`, it will generate either `Some` or `None` based on whether the input is
   *  `Some` or `None`. 
   *  The `Some` case will contain a list of length `n` that repeats the element in the input `Option`.
   *  
   *  The general meaning of `replicateM` is described well by the implementation
   *   `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value `n` times 
   *   and gathers the results in a single value, 
   *   where the monad `F` determines how values are actually combined.
   * 
   */
  def replicateM[A](n:Int,ma:F[A]):F[List[A]] = map(ma)(a => List.fill(n)(a))
  
  def filterM[A](ms:List[A])(f: A => F[Boolean]):F[List[A]] = ms.foldRight(unit(List[A]()))((a, mla) => {
    map2(f(a),mla)((bool,la) => if(bool) { a :: la} else {la})
  })
  
  def compose[A,B,C](f: A => F[B],g:B => F[C]): A => F[C] = {a => flatMap(f(a))(b => g(b))}
  
  def flatMap_v2[A,B](ma:F[A])(f: A => F[B]):F[B] = compose((_:Unit) => ma, f)(())
  
  def join[A](mma:F[F[A]]):F[A] = flatMap(mma)(ma => ma)
}

object MonadUtils {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A):Gen[A]  = Gen.unit(a)
    def flatMap[A,B](ma:Gen[A])(f: A => Gen[B]):Gen[B] = ma flatMap f 
  }
  
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A):Option[A]  = Option(a)
    def flatMap[A,B](ma:Option[A])(f: A => Option[B]):Option[B] = ma flatMap f 
  }
  
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A):Stream[A] = Stream(a)
    def flatMap[A,B](ma:Stream[A])(f: A => Stream[B]):Stream[B] = ma flatMap f
  }
  
  val listMonad = new Monad[List] {
    def unit[A](a: => A):List[A] = List(a)
    def flatMap[A,B] (ma:List[A])(f: A => List[B]):List[B] = ma flatMap f
  }
  
  val idMonad = new Monad[Id] {
    def unit[A](a: => A):Id[A] = Id(a)
    def flatMap[A,B](ma:Id[A])(f: A => Id[B]):Id[B] = ma flatMap f
  }
}

