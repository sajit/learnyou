package com.example

/**
 * Created by sajit on 5/2/15.
 */
case class Foo(name:String){
  
  def transform():String = this.name.toUpperCase()
}
