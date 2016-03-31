package com.example

trait MyFactory[A] {
  def calc(a:A):A
}
case class FactoryBoy[A](f: (A => A)) extends MyFactory[A] {
  def calc(a:A):A = f(a)
}
