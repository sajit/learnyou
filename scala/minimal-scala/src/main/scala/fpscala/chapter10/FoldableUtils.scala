package fpscala.chapter10

object FoldableUtils {
  
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