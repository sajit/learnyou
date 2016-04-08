package fpscala.chapter8


import fpscala.BaseSpec
class JphmSpec extends BaseSpec { 
  
  it should " be an example of God's mercy on sinners" in {
    true should be(true)
  }
  
  class FooProp extends Prop {
    def check = true
  }
  it should "have a defined function check" in {
    val prop1 = new FooProp()
    prop1.check should be(true)
  }
  
  val simpleRng = RNG.Simple(100)
  
  it should "choose a number less than second arg" in {
    
    val genNum:Gen[Int] = Gen.choose2(1, 8)
    val foo = genNum.sample.run
    val (anInt,aRng) = foo.apply(simpleRng)
    anInt should be < (8)
  }
  
  it should "always get a value" in {
    val genNum:Gen[Int] = Gen.unit(3)
    val foo = genNum.sample.run
    val (anInt,aRng) = foo.apply(simpleRng)
    anInt should be (3)
  }
  
  it should "generate a list of length n" in {
    val g =  Gen.choose2(0,3)
    val listGen = Gen.listOfN2(5, g)
    val foo = listGen.sample.run
    val result = foo.apply(simpleRng)
    result._1.length should be(5)
    result._1.foreach { x => x should be < (3) }
  }
  
  it should "generate a 5 element list with hellos" in {
    val listS = Gen.largeStringsList(5)
    val foo = listS.sample.run
    val (rlist,rng) = foo.apply(simpleRng)
    rlist.length should be (5)
    rlist.foreach(word => word should be ("Hello"))
    
  }
  
  it should "flatMap a generator of Int to Boolean" in {
    val genInt = Gen.unit(3)
    val fm1:(Int => Gen[Boolean]) = { x => Gen.unit(x%2 != 0)}
    val foo = genInt.flatMap(fm1).sample.run
    val (aBool,aRng) = foo.apply(simpleRng)
    aBool should be (true)
    val genInt1 = Gen.unit(4)
    val (bBool,bRng) = genInt1.flatMap(fm1).sample.run.apply(simpleRng)
    bBool should be (false)
  }
  
  it should "unitify a state" in {
    
    val (result,rng) = State.unit(5).run.apply(simpleRng)
    result should be (5)
  }
  
  it should "show Jesus' mercy on the world" in {
    val genInt = Gen.unit(3)
    val genIntList = Gen.listOfN3(genInt)
    val (result,rng):(List[Int],RNG) = genIntList.sample.run(simpleRng)
    result.head should be (5)
    result.length should be (3)
    
    
  }
  
  it should "drops of mercy" in {
    val genInt = Gen.unit(3)
    val genIntList = Gen.listOfN4(4,genInt)
    val (result,rng):(List[Int],RNG) = genIntList.sample.run(simpleRng)
    result.head should be (3)
    result.length should be (4)
  }
  
}