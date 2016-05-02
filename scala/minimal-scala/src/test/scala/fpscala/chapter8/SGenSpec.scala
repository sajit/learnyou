package fpscala.chapter8

import fpscala.BaseSpec

class SGenSpec  extends BaseSpec{


  val runner: (RNG) => (Int, RNG) = {rng => rng.nextInt}

  val state: State[RNG, Int] = State[RNG,Int](runner)

  it should " generate a SGen from Gen" in {
    val gen = Gen(state)
    val sGen:SGen[Int] = gen.unsized
    val derivedGen = sGen.forSize(10)
    derivedGen should be (gen)
  }

  it should "apply function behave as usual " in {
    def foo: (Int => Gen[Int]) = { x => Gen.unit(x) }
    val sgen = SGen(foo)
    val aGen: Gen[Int] = sgen.apply(5)
    val simpleRng = RNG.Simple(100)
    val (result, aRng) = aGen.sample.run(simpleRng)
    result should be(5)
  }
}
