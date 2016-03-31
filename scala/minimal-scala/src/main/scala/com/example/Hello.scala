package com.example

object Hello {
  def main(args: Array[String]): Unit = {
    System.out.println("Hello, world!")
  }

  @annotation.tailrec
  def mfl[A,B](as:List[A],z:B)(f:(B,A) => B): B = as match{
    case Nil => z
    case _ => {
      mfl(as.init,f(z,as.last))(f)

    }
  }

  def mReverse[A](as:List[A]):List[A] = as match {
    case Nil => Nil
    case x::xs => mReverse(xs) ++ List(x)
  }
}
