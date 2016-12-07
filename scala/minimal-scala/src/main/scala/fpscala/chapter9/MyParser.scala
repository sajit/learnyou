package fpscala.chapter9




case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

}

case class ParseError(stack:List[(Location,String)]) {
  def push(location:Location,message:String):ParseError = ParseError((location,message) :: stack)


  def label[A](s:String):ParseError = ParseError((stack.lastOption map (_._1)).map((_,s)).toList)
}

sealed trait Result[+A]{

  def extract: Either[ParseError,A] = this match {
    case Failure(e) => Left(e)
    case Success(v,_) => Right(v)
  }
}
case class Success[+A](get:A,charsConsumed:Int) extends Result[A]
case class Failure(get:ParseError) extends Result[Nothing]

/** A parser is a kind of state action that can fail. */
object Utils{
  type Parser[+A] = Location => Result[A]

  def string(w: String): Parser[String] = { location =>
     val msg = "Not match"
     if(location.input.equals(w)){
       Success(w,w.length)
     }
     else {
       Failure(ParseError(List((location,msg))))
     }

  }
}



