package fpscala.chapter12

import fpscala.chapter11.Functor
import java.util.Date

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
    
  def map3[A,B,C,D](fa:F[A],fb:F[B],fc:F[C])(f:(A,B,C) => D):F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  
  def map4[A,B,C,D,E](fa:F[A],fb:F[B],fc:F[C],fd:F[D])(f:(A,B,C,D) => E):F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  
   def product[G[_]](G:Applicative[G]):Applicative[({type f[x] = (F[x],G[x])})#f] = {
      val self = this
        new Applicative[({type f[x] = (F[x], G[x])})#f] {
                  def unit[A](a: => A) = (self.unit(a), G.unit(a))
                  def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(fn: (A, B) => C):(F[C], G[C]) = (self.map2(fa._1, fb._1)(fn),G.map2(fa._2,fb._2)(fn))
                  override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
                              (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
      }
  }
  
  def compose[G[_]] (G: Applicative[G]):Applicative[({type f[x] = F[G[x]]})#f] =  {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A] (a: => A) = self.unit(G.unit(a))
      def map2[A,B,C](fga:F[G[A]],fgb:F[G[B]])(fn:(A,B) => C):F[G[C]] = self.map2(fga, fgb)((ga,gb) => G.map2(ga, gb)((a,b) => fn(a,b)))
    }
  }
  
  def sequenceMap[K,V](ofa:Map[K,F[V]]):F[Map[K,V]] = ofa.foldRight(this.unit(Map[K,V]()))((el,facc) => map2(el._2,facc)((v,acc) => acc + (el._1 -> v)))
}

sealed trait Validation[+E,+A]

case class Failure[E](head:E,tail:Vector[E] = Vector()) extends Validation[E,Nothing] 

case class Success[A](a:A) extends Validation[Nothing,A]


case class WebForm(name:String,birthdate:Date,phoneNumber:String)

object Applicative {
  
  val listApplicative = new Applicative[List] {
    def unit[A](a: => A):List[A] = List(a)
    def map2[A,B,C](fa:List[A],fb:List[B])(f:(A,B) => C):List[C] = fa zip fb map (tuple => f(tuple._1,tuple._2))
  }
  
  val optionApplicative = new Applicative[Option] {
    def unit[A](a: => A):Option[A] = Some(a)
    def map2[A,B,C](fa:Option[A],fb:Option[B])(f:(A,B) => C):Option[C] = (fa,fb) match {
      case (Some(a),Some(b)) => Some(f(a,b))
      case _ => None 
    }
  }
  

  
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
  new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: => A) = Success(a)
    override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C) = (fa,fb) match {
      case (Success(a),Success(b)) => Success(f(a,b))
      case (Success(a),Failure(hb,tb)) => Failure(hb,tb)
      case (Failure(ha,ta),Success(b)) => Failure(ha,ta)
      case (Failure(ha,ta),Failure(hb,tb)) => Failure(ha,ta ++ Vector(hb) ++ tb)
    }
  }
  
  def validName(name:String):Validation[String,String] = 
    if(name == null || name.isEmpty()) Failure("Name cannot be empty") else Success(name)
    
  def validBirthDate(birthDate:String):Validation[String,Date] = 
      try{
      import java.text._
      val date:Date = new SimpleDateFormat("yyyy-MM-dd").parse(birthDate)
      Success(date)
      }
      catch {
        case _:Throwable => Failure("Bad date format")
      }
     
    def validPhone(phone:String):Validation[String,String] = 
      if(phone.length()==10)Success(phone) else Failure("wrong format")
      
    def validWebForm(name:String,bday:String,phone:String):Validation[String,WebForm] = 
      validationApplicative.map3(validName(name),validBirthDate(bday),validPhone(phone))(WebForm(_,_,_))
    
      
    
}