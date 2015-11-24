package fpscala.chapter6

import scala.util.Random


/**
 * Created by sajit.kunnumkal on 11/16/2015.
 */
object PreludeRNGs {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val tuple = rng.nextInt
    tuple._1 match {
      case Int.MinValue => (Math.abs(tuple._1 + 1), tuple._2)
      case _ => (Math.abs(tuple._1), tuple._2)
    }
  }

  def double(rng:RNG): (Double,RNG) = {
    val tuple = nonNegativeInt(rng)
    tuple._1 match {
      case Int.MaxValue => (Int.MaxValue-1.toDouble/Int.MaxValue.toDouble,tuple._2)
      case i => (i.toDouble/Int.MaxValue.toDouble,tuple._2)
    }
  }

  def intDouble(rng:RNG):((Int,Double),RNG) = {
    val (anInt,nRng) = rng.nextInt
    val (aDouble,nnRng) = double(nRng)
    ((anInt,anInt.toDouble),nnRng)
  }

  def doubleInt(rng:RNG):((Double,Int),RNG) = {
    val ((anInt,aDouble),nRng) = intDouble(rng)
    ((aDouble,anInt),nRng)
  }


  def double3(rng:RNG): ((Double,Double,Double),RNG) = {
    val (d1,nRng) = double(rng)
    val (d2,n2Rng) = double(nRng)
    val (d3,n3Rng) = double(n2Rng)
    ((d1,d2,d3),n3Rng)
  }

  def ints(count:Int)(rng:RNG):(List[Int],RNG) = {
    def doInts(rem:Int,acc:(List[Int],RNG)):(List[Int],RNG) = {
      if(rem <= 0){
        acc
      }
      else {
        val nextRandom = acc._2.nextInt
        doInts(rem-1,(nextRandom._1 :: acc._1, nextRandom._2))
      }
    }
    doInts(count,(List(),rng))
  }

  type Rand[+A] = RNG => (A,RNG)

  val int:Rand[Int] = _.nextInt
  val doublex:Rand[Double] = {rng => double(rng)}

  def unit[A](a:A):Rand[A] = rng => (a,rng)

  def map[A,B](s:Rand[A])(f: A => B):Rand[B] = rng => {
    val (a,rng2) = s(rng)
    (f(a),rng2)
  }

  def nonNegativeEven:Rand[Int]  = map(nonNegativeInt) {i => if(i%2 ==0) {i}  else {i-1}}

  def elegantDouble : Rand[Double] = map(nonNegativeInt) { i => if(i == Int.MaxValue)
    Int.MaxValue - 1 else i

  }

  def moreElegantDouble : Rand[Double] = rng => double(rng)

  def map2[A,B,C](action1:Rand[A],action2:Rand[B])(f: (A,B) => C):Rand[C] = rng => {
    val (a,rng2) = action1(rng)
    val (b,rng3) = action2(rng2)
    (f(a,b),rng3)
  }

  def both[A,B](ra:Rand[A],rb:Rand[B]):Rand[(A,B)] = map2(ra,rb)((_,_))

  val randIntDouble: Rand[(Int,Double)] = both(int,doublex)

  val randDoubleInt:Rand[(Double,Int)] = both(doublex,int)

  /**
   * Copied from github
   */
  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}
