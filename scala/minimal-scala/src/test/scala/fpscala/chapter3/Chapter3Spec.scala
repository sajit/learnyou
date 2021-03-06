package fpscala.chapter3

import org.scalatest.{Matchers, FlatSpec}


/**
 * Created by sajit on 9/9/15.
 */
class Chapter3Spec extends FlatSpec with Matchers {


    it  should " apply a function in foldRight " in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      val sum = BList.foldRight(bList,0)((x,y) => x + y)
      sum should be  (14)
    }

   it should " calculate length" in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      BList.length(bList) should be (4)
    }
    it should "fold left sum" in {
      val bList = BCons(4,BCons(5,BCons(3,BCons(2,BNil))))
      val sum = BList.foldLeft(bList,0)((x,y) => x + y)
      sum should be  (14)
    }

  it should "fold left product" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val product = BList.foldLeft(bList,1)((x,y) => x * y)
    product should be  (-120)
  }

  it should "reverse" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val reverse = BList.reverse(bList)
    BList.length(reverse) should be  (BList.length(bList))
    reverse should be  (BCons(2,BCons(3,BCons(-5,BCons(4,BNil)))))
  }

  it should "reverse from a foldLeft" in {
    val bList = BCons(4,BCons(-5,BCons(3,BCons(2,BNil))))
    val reverse = BList.revV2(bList)
    BList.length(reverse) should be  (BList.length(bList))
    reverse should be  (BCons(2,BCons(3,BCons(-5,BCons(4,BNil)))))
  }

  it should "append using foldRight" in {
    val a1 = BCons(2,BCons(4,BCons(6,BNil)))
    val a2 = BCons(-3,BCons(-5,BNil))
    val result = BList.appendv2(a1,a2)
    BList.length(result) should be (5)
    result should be  (BCons(2,BCons(4,BCons(6,BCons(-3,BCons(-5,BNil))))))
  }

  it should "flatten a list of lists " in {
    val a1 = BCons(2,BCons(4,BCons(6,BNil)))
    val a2 = BCons(-3,BCons(-5,BNil))

    val in:BList[BList[Int]] = BCons(a1,BCons(a2,BNil))
    val result = BList.myFlatMap(in)
    BList.length(result) should be  (5)
    result should be  (BCons(2,BCons(4,BCons(6,BCons(-3,BCons(-5,BNil))))))
  }

  it should "do lfatMapping" in {
    val inp = BList(1,2,3)
    val result = BList.flatMap(inp)( i => BList(i,i))
    result should be (BList(1,1,2,2,3,3))
  }

  it should "add up for equal lengths" in {
    val result = BList.addup(BList(1,2,3),BList(4,5,6))
    result should be (BList(5,7,9))
  }

  it should "throw up" in {
    intercept[RuntimeException]{
      BList.addup(BList(1,3,4),BNil)
    }

  }

  it should " zip with " in {
    val a1:BList[Int] = BList(1,2,3)
    val a2:BList[Int] = BList(-1,3,2)
    val f:(Int,Int) => Int = (x,y) => x*y
    val result = BList.zipWith(a1,a2,f)
    result should be (BList(-1,6,6))
  }

  it should " find subsequences " in {
    val sup = BList(1,2,3,4)
    BList.hasSubsequence(sup,BList(1,2)) should be (true)
    BList.hasSubsequence(sup,BList(2,3)) should be (true)
    BList.hasSubsequence(sup,BList(4)) should be (true)
    BList.hasSubsequence(sup,BList(4,1)) should be (false)
    BList.hasSubsequence(sup,BList(3,2)) should be (false)
  }

  it should "count nodes in a CTree" in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    CTree.size(tree) should be (5)
  }

  it should "get max depth in a CTree "  in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    CTree.depth(tree) should be (3)
  }

  it should "double values  in a CTree " in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    val result = CTree.map[Int,Int](tree, x => x*2)
    result should be (Branch(Branch(Leaf(6),Leaf(10)),Leaf(4)))
  }

  it should "implement size as fold " in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    val result = CTree.fold[Int](tree,((x,y) => (x+y+1)), (_ => 1))
    result should be (5)
  }

  it should "implement maximum as fold " in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    val result = CTree.fold[Int](tree,((x,y) => x max y), (j => j))
    result should be (5)
  }

  it should "implement depth as fold " in {
    val tree = Branch(Branch(Leaf(3),Leaf(5)),Leaf(2))
    val result = CTree.fold[Int](tree,((x,y) => (x max y)+1), (_ => 1))
    result should be (3)
  }
}
