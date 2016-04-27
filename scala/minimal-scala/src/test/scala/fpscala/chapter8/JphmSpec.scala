package fpscala.chapter8

import fpscala.BaseSpec
import fpscala.chapter8.Prop._
class JphmSpec extends BaseSpec { 
  
  it should " be an example of God's mercy on sinners" in {
    true should be(true)
  }
  val simpleRng = RNG.Simple(100)
  
  val aRun:(TestCases,RNG)=> Result = {
    case (0,simpleRng) => Passed
    case _ => Passed
  }
  class FooProp extends Prop(aRun){
    def check = true
  }
  
  
  it should "have a defined function check" in {
    
    val prop1 = new FooProp()
    prop1.check should be(true)
  }
  
  
  
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
  
  it should "reiterate to relearn" in {
    val genInt = Gen.unit(3)
    val listN4Gen = Gen.intListOfN(4, genInt)
    val (result,rng):(List[Int],RNG) = listN4Gen.sample.run(simpleRng)
    
    result.length should be (4)
    result.foldLeft(3)((acc,curr) => {
      curr should be <= (acc)
      curr
    })
    
  }
  
  it should "union generators " in {
    val g1 = Gen.unit(1)
    val g2 = Gen.unit(2)
    val g3 = Gen.unit(3)
    val poller = Gen.boolean.sample.run(simpleRng)
    val (result,rng) = Gen.union(Gen.union(g1,g2),g3).sample.run(simpleRng)
    result should not be (2)
//    if(poller._1){
//      result should be (1)
//    }
//    else{
//      result should be (3)
//    }
  }
  
  it should "only pass if both props pass " in {
    
    val p1 = Prop({(n,rng) => Passed})
    val p2 = Prop((n,rng) => Passed)
    p1.&&(p2).run.apply(1, simpleRng) should be (Passed)
  }
  
  it should "not pass if either fail " in {
    val failed = Falsified("tough luck",1)
    val p1 = Prop({(n,rng) => failed})
    val p2 = Prop((n,rng) => Passed)
    p1.&&(p2).run.apply(1, simpleRng) should be (failed)
  }
  
  it should "pass if either pass " in {
    val failed = Falsified("tough luck",1)
    val p1 = Prop({(n,rng) => failed})
    val p2 = Prop((n,rng) => Passed)
    p1.||(p2).run.apply(1, simpleRng) should be (Passed)
  }

  it should "build a message" in {
    val result = Prop.buildMsg(1, new Exception("hello world"))
    result should be("test case: 1\n" + "generated an exception: hello world")
  }

  it should "generate a stream " in {
    val g:Gen[Int] = Gen.unit(4)
    val intStream:Stream[Int] = Prop.randomStream(g)(simpleRng)
    intStream.isEmpty should be (false)
    intStream.head should be (4)
  }
  
}