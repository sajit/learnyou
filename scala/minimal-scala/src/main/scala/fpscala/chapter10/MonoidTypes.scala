package fpscala.chapter10

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
}