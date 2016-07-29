package fpscala.chapter10

object MonoidTypes {
  val stringMonoid = new Monoid[String] {
    def op(a1:String,a2:String) = a1 + a2
    val zero = ""
  }
}