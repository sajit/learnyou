package fpscala.chapter9

import fpscala.BaseSpec
import fpscala.chapter9.Utils.Parser

class MyParserSpec extends BaseSpec{

  it should "apply string function " in {
    val location = Location("hello",0)
    Utils.string("hello")(location) should be (Success("hello","hello".length))
    Utils.string("world")(location) should be (Failure(ParseError(List((location,"Not match")))))


  }

  it should "parse till end " in {
    val string ="hellohellohello"
    val location1 = Location(string.substring(0,5),0)
    Utils.string("hello")(location1) should be (Success("hello",5))
    val location2 = Location(string.substring(5,10),5)
    Utils.string("hello")(location2) should be (Success("hello",5))
    val location3 = Location(string.substring(10,15),10)
    Utils.string("hello")(location3) should be (Success("hello",5))

  }
  it should "parse till first failure " in {
    val string ="hellohellohell"
    val location1 = Location(string.substring(0,5),0)
    Utils.string("hello")(location1) should be (Success("hello",5))
    val location2 = Location(string.substring(5,10),5)
    Utils.string("hello")(location2) should be (Success("hello",5))
    val location3 = Location(string.substring(10,14),10)
    Utils.string("hello")(location3) should be  (Failure(ParseError(List((location3,"Not match")))))

  }

}
