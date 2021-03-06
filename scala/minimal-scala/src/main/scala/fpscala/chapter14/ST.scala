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

sealed abstract class STArray[S,A](implicit manifest:Manifest[A]) {



  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S,A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] =
    xs.foldRight(ST[S,Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()


}
object STArray {
  def apply[S,A:Manifest](sz:Int,v:A):ST[S,STArray[S,A]] =
    ST(new STArray[S,A]() {
      val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs:List[A]):ST[S,STArray[S,A]] = ST(new STArray[S,A]() {
    val value = xs.toArray
  })


}
