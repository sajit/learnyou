package file.io

import scala.util.{Success, Try}

object PipeLiningExample {

  def parse(aStr: String): Try[Double] = Try(aStr.toDouble)


  def toCelsius(f: Double): Double = (5.0/9.0)*(f-32.0)

  /**
    * convert a stream of strings to a stream of doubles
    *  rules
    *   only doubles between 45 and -100
    *
    * @param s
    * @return
    */
  def convert(s:Stream[String]):Stream[Double] = {
     s.map(str => parse(str)).
       collect{case Success(d) => d}.filter(p => p <= 45 && p >= -100).
       map(toCelsius(_))

  }
}
