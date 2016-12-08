package file.io

import scala.io.Source
import scala.util.Try

/**
  * Created by sajit on 11/27/16.
  */
object FileUtils {

  val file = "/temp.txt"
  val stream = Source.fromFile(file).getLines().toStream

  def safeMap(s:String):Try[Double] = {
    Try(s.toDouble)
  }
  def main(args:Array[String]) = {
    stream.map(str => str.toDouble)
  }

}
