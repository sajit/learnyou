package fpscala.chapter11


case class MyState[S,A] (run: S => (A,S)) {
  def map[B](f: A => B):MyState[S,B] =
    MyState(s => {
      val (a,s1)  = run(s)
      (f(a),s1)
    })
    
  def flatMap[B](f: A => MyState[S,B]): MyState[S,B] = 
    MyState(s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    })
    
}
object MyStateUtils {
  type IntState[A]  = MyState[Int,A]
  
  def stateMonad[S] = new Monad[({type f[x] = MyState[S,x]})#f]{
    def unit[A](a: => A):MyState[S,A] = MyState(s => (a,s))
    
    def flatMap[A,B](st:MyState[S,A])(f:A => MyState[S,B]): MyState[S,B] = st flatMap f
  }
}
