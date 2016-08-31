package fpscala.chapter10

import fpscala.chapter3._

object FoldableUtils {
 
  
  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
                            b.getOrElse(k, V.zero)))
      }
  }
  
   /**
   * Use monoids to build a bag from an indexed Seq : Directly copied
   */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = MonoidTypes.foldMapV(as, mapMergeMonoid[A, Int](MonoidTypes.intAddition))((a: A) => Map(a -> 1))

}
class FoldableList extends Foldable[List] {
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: List[A])(f: A => B)(mn: Monoid[B]): B = as.foldRight(mn.zero){(a,b) => mn.op(b,f(a))}

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
}

class FoldableIndexedSeq extends Foldable[IndexedSeq] {
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mn: Monoid[B]): B = as.foldRight(mn.zero){(a,b) => mn.op(b, f(a))}

  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
}

class FoldableStream extends Foldable[Stream] {
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  def foldMap[A, B](as: Stream[A])(f: A => B)(mn: Monoid[B]): B = as.foldRight(mn.zero)((a,b) => mn.op(b, f(a)))

  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
}

class FoldableTree extends Foldable[CTree] {
  def foldLeft[A, B](as: CTree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(x) => z
    case Branch(left,right) => {
      foldLeft(right)(foldLeft(left)(z)(f))(f)
    }
  }

  def foldMap[A, B](as: CTree[A])(f: A => B)(mn: Monoid[B]): B = foldRight(as)(mn.zero)((a,b) => mn.op(b,f(a)))

  def foldRight[A, B](as: CTree[A])(z: B)(f: (A, B) => B): B = as match {
   case Leaf(x) => z
    case Branch(left,right) => {
      foldRight(left)(foldRight(right)(z)(f))(f)
    }
  }
}

class FoldableOption extends Foldable[Option] {
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case Some(a) => f(z,a)
    case None => z
  }

  def foldMap[A, B](as: Option[A])(f: A => B)(mn: Monoid[B]): B = foldRight(as)(mn.zero)((a,b) => mn.op(b,f(a)))

  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case Some(a) => f(a,z)
    case None => z
  }
}