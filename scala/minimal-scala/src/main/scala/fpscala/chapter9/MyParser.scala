package fpscala.chapter9

import scala.util.matching.Regex


case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

}

case class ParseError(stack:List[(Location,String)]) {
  def push(location:Location,message:String):ParseError = copy(stack = (location,message) :: stack)


  def label[A](s:String):ParseError = ParseError((stack.lastOption map (_._1)).map((_,s)).toList)
}

sealed trait Result[+A]{

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }

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
     if(location.input.startsWith(w)){
       Success(w,w.length)
     }
     else {
       Failure(ParseError(List((location,msg))))
     }

  }

  def succeed[A](a: A): Parser[A] = {loc => Success(a,0)}

  def slice[A](p: Parser[A]): Parser[String] = {
    loc => p(loc) match {
      case Success(_,n) => Success(loc.input.substring(0,n),n)
      case Failure(f) => Failure(f)
    }
  }


  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    loc =>
      r.findPrefixOf(loc.input) match {
        case None => Failure(ParseError(List((loc,msg))))
        case Some(m) => Success(m,m.length)
      }
  }


  def scope[A](msg:String)(p:Parser[A]):Parser[A] = location => p(location).mapError(_.push(location,msg))

  def label[A](msg:String)(p:Parser[A]):Parser[A] = location => p(location).mapError(_.label(msg))
}



