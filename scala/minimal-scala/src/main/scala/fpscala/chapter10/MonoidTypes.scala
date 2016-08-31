package fpscala.chapter10

import fpscala.chapter7.NonBlocking._

object MonoidTypes {
  val stringMonoid = new Monoid[String] {
    def op(a1:String,a2:String) = a1 + a2
    val zero = ""
  }
  
  val intAddition = new Monoid[Int] {
    def op(a1:Int,a2:Int) = a1 + a2
    val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(a1:Int,a2:Int) = a1 * a2
    val zero = 1
  }
  
  val booleanOr = new Monoid[Boolean] {
    def op(a1:Boolean,a2:Boolean) = a1 || a2
    val zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1:Boolean,a2:Boolean) = a1 && a2
    val zero = true
  }
  
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1:Option[A],a2:Option[A]):Option[A] = a1.orElse(a2)
    val zero = None
  }
  
  // There is a choice of implementation here as well.
// Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
// We can then get the other one using the `dual` construct (see previous answer).
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = {x:A => x}
  }
  
  sealed trait WC 
  case class Stub(chars: String) extends WC
  case class Pair(lStub:String,words:Int,rStub:String) extends WC
  
  val wcMonoid:Monoid[WC] = new Monoid[WC] {
    def op(a1:WC,a2:WC):WC = (a1,a2) match {
      case (l:Pair,r:Pair) => Pair(l.lStub,
          l.words + (if((l.rStub+r.lStub).isEmpty()) 0 else 1) + r.words,
          r.rStub) 
      case (Stub(c),r:Pair) => Pair(c+r.lStub,r.words,r.rStub)
      case (l:Pair,Stub(c)) => Pair(l.lStub,l.words,l.rStub+c)
      case (Stub(a),Stub(b)) => Stub(a+b)
    }
    val zero = Stub("")
  }
  
  def concatenate[A] (as:List[A],m:Monoid[A]):A = as.foldLeft(m.zero)(m.op)
  
  def foldMap[A,B] (as:List[A],m:Monoid[B])(f: A => B):B = as.foldLeft(m.zero) {  (acc,el) => m.op(f(el),acc)}
  
  def foldMapV[A,B](v:IndexedSeq[A],m:Monoid[B])(f: A => B):B = {
    if(v.length ==0){
      m.zero
    }
    else if(v.length == 1) {
      f(v(0))
    }
    else {
      val (left,right ) = v.splitAt(v.length/2)
      m.op(foldMapV(left,m)(f),foldMapV(right,m)(f))
    }
    
  }
  
  def par[A](m:Monoid[A]):Monoid[NBPar[A]] = new Monoid[NBPar[A]] {
    def op(a1:NBPar[A],a2:NBPar[A]) = map2(a1,a2)(m.op)
    val zero = unit(m.zero)
  }
  
  def parFoldMap[A,B](v:IndexedSeq[A],m:Monoid[B])(f:A => B):NBPar[B] = {
    if(v.length ==0){
      unit(m.zero)
    }
    else if(v.length == 1) {
      unit(f(v(0)))
    }
    else {
      val (left,right ) = v.splitAt(v.length/2)
      map2(parFoldMap(left,m)(f),parFoldMap(right,m)(f))(m.op)
     
    }
    
  }
  
  def productMonoid[A,B] (A:Monoid[A], B: Monoid[B]):Monoid[(A, B)] = new Monoid[(A,B)] {
    def op(p1:(A,B),p2:(A,B)) = (A.op(p1._1,p2._1),B.op(p1._2,p2._2))
    val zero = (A.zero,B.zero)
  }
  
  
}