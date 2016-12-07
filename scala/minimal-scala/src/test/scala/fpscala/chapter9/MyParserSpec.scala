package fpscala.chapter9

import fpscala.BaseSpec
import fpscala.chapter9.Utils.Parser

class MyParserSpec extends BaseSpec{

  it should "apply string function " in {
    val location = Location("hello",0)
    Utils.string("hello")(location) should be (Success("hello","hello".length))
    Utils.string("world")(location) should be (Failure(ParseError(List((location,"Not match")))))


  }

}
