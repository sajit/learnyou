
package com.example

/**
 * Created by sajit on 5/2/15.
 */
case class Foo(name:String){
  def reverse():String = name.reverse
  def concat(s:String):String = name + s
  def transform():String = this.name.toUpperCase()
}

