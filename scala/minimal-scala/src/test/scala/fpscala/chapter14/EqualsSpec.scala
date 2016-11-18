package fpscala.chapter14

import fpscala.BaseSpec

class EqualsSpec extends BaseSpec{

  case class Foo(name:String)

  it should "highlight diff b/w == and eq" in {
    Foo("hello") == Foo("hello") should be (true)
    Foo("hello") eq Foo("hello") should be (false)
  }

}
