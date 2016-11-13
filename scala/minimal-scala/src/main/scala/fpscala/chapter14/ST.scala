package fpscala.chapter14

/**
  * A datatype to capture scoped mutation. This should offer two static guarantees. Code using this data type
  * should not compile if violation of following
  * a. If we hold a reference to a mutable object , then nothing can observe us mutating it
  * b. A mutable obejct can never be observed outside of the scope in which it was created
  *
  * Created by sajit on 11/13/16.
  */

sealed trait ST[S,A] { self =>
  protected  def run(s:S):(A,S)
  def map[B](f: A => B):ST[S,B] = new ST[S,B] {
    def run(s:S) = {
      val (a,s1) = self.run(s)
      (f(a),s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]):ST[S,B] = new ST[S,B] {
    def run(s:S) = {
      val (a,s1) = self.run(s)
      f(a).run(s1)
    }
  }

}
object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s:S) = (memo,s)
    }
  }
}

sealed trait STRef[S,A] {
  protected var cell:A
  def read:ST[S,A] = ST(cell)
  def write(a:A):ST[S,Unit] = new ST[S,Unit] {
    def run(s:S) = {
      cell = a
      ((),s)
    }
  }
}
object STRef {
  def apply[S,A](a:A):ST[S,STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}