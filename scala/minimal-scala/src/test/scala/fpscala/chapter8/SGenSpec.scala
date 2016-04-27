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
}
