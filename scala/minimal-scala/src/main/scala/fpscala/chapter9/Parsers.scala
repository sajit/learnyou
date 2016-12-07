import scala.util.matching.Regex

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

trait Parsers[Parser[+_]] {

  def run[A](p:Parser[A])(input:String):Either[ParseError,A]

  def string(s:String):Parser[String]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A]


  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)
  def char(c:Char):Parser[Char] = map(string(c.toString))(_.charAt(0))

  def regex[A](r:Regex):Parser[String]

  def slice[A](p:Parser[A]):Parser[String]


}