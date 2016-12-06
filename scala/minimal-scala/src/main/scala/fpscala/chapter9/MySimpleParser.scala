package fpscala.chapter9

import scala.collection.mutable

object MySimpleParser {

  def getProperty(stack: mutable.Stack[Char]):String = {
    var popped = Array[Char]()
    var char:Char = stack.pop()
    while(char != '{'){
      popped = popped.+(char)
      char = stack.pop()
    }
    popped.toString().reverse
  }

  def getKeyValue(stack: mutable.Stack[Char]):(String,String) = {
    val rawStr  = getProperty(stack)
    val strs = rawStr.split(":")
    (strs(0),strs(1))
  }

  def parseString(str:String):Map[String,Object] = {
    def doParse(idx:Int,jsObj:Map[String,Object],stack:mutable.Stack[Char]):(Int,Map[String,Object]) = {
      if(idx>=str.length){
        if(!stack.isEmpty){
          throw new Exception
        }
        else{
          (idx,jsObj)
        }
      }
      else{
        val ch:Char = str.charAt(idx)
        if(ch.isLetterOrDigit || ch == ':'){
          doParse(idx+1,jsObj,stack.push(ch))
        }
        else if(ch == '{'){
            val (parseIdx,child) = doParse(idx+1,Map[String,Object](),stack.push(ch))
            val key:String = getProperty(stack)
            doParse(parseIdx + 1,jsObj + (key -> child),stack)
        }
        else if(ch == ','){
          val (key,value):(String,String)  = getKeyValue(stack)
          doParse(idx + 1,jsObj + (key,value),stack)
        }
        else if(ch  == '}'){
          stack.pop(); //remove preivoulsy '{'
          (idx+1,jsObj)
        }
        else {
          doParse(idx + 1,jsObj,stack)
        }
      }
    }
    doParse(0,Map[String,Object](),new mutable.Stack[Char]())

  }

}
