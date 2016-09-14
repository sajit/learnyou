package fpscala.chapter9
case class ParseError(){}
trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
 
  def map[A,B](a:Parser[A])(f: A => B):Parser[B] 
  def extract[A](a:Parser[A]):A
  
  def map2[A,B,C](p: Parser[A],p2:Parser[B])(f:(A,B) => C):Parser[C] = map(p)(a => extract(map(p2)(b => f(a,b)))) 
  
  def product[A,B](p:Parser[A],p2:Parser[B]):Parser[(A,B)] = map2(p,p2)((a,b) => (a,b))
  
}
//case class Location(input: String, offset: Int = 0) {
//
//  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
//  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
//    case -1 => offset + 1
//    case lineStart => offset - lineStart
//  }
//
//  def toError(msg: String): ParseError =
//    ParseError(List((this, msg)))
//
//  def advanceBy(n: Int) = copy(offset = offset+n)
//
//  /* Returns the line corresponding to this location */
//  def currentLine: String =
//    if (input.length > 1) input.lines.drop(line-1).next
//    else ""
//
//  def columnCaret = (" " * (col-1)) + "^"
//}
//case class ParseError(stack: List[(Location,String)] = List()) {
//  def push(loc: Location, msg: String): ParseError =
//    copy(stack = (loc,msg) :: stack)
//
//  def label[A](s: String): ParseError =
//    ParseError(latestLoc.map((_,s)).toList)
//
//  def latest: Option[(Location,String)] =
//    stack.lastOption
//
//  def latestLoc: Option[Location] =
//    latest map (_._1)
//
//  /**
//  Display collapsed error stack - any adjacent stack elements with the
//  same location are combined on one line. For the bottommost error, we
//  display the full line, with a caret pointing to the column of the error.
//  Example:
//  1.1 file 'companies.json'; array
//  5.1 object
//  5.2 key-value
//  5.10 ':'
//  { "MSFT" ; 24,
//  */
//  override def toString =
//    if (stack.isEmpty) "no error message"
//    else {
//      val collapsed = collapseStack(stack)
//      val context =
//        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
//        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
//      collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
//      context
//    }
//
//  /* Builds a collapsed version of the given error stack -
//   * messages at the same location have their messages merged,
//   * separated by semicolons */
//  def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
//    s.groupBy(_._1).
//      mapValues(_.map(_._2).mkString("; ")).
//      toList.sortBy(_._1.offset)
//
//  def formatLoc(l: Location): String = l.line + "." + l.col
//}
object Parsers {
  
}