
package com.example

/**
 * Created by sajit on 5/2/15.
 */
case class Foo(name:String){
  def reverse() = name.reverse
  def concat(s:String) = name + s
  def transform():String = this.name.toUpperCase()
}

