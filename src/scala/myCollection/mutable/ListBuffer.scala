package scala.myCollection.mutable

import scala.collection.parallel.immutable
import scala.myCollection.{IterableOnce, StrictOptimizedSeqOps}

class ListBuffer[A]
  extends AbstractBuffer[A]
    with SeqOps[A, ListBuffer, ListBuffer[A]]
    with StrictOptimizedSeqOps[A, ListBuffer, ListBuffer[A]]
    with ReusableBuilder[A, immutable.List[A]] {

  private var first: List[A] = Nil
  private var last0: ::[A] = null

}

object ListBuffer {

  def from[A](coll: IterableOnce[A]): ListBuffer[A] = new ListBuffer[A] ++= coll

  def newBuilder[A] : Builder[A, ListBuffer[A]] = new GrowableBuilder(empty[A])

  def empty[A]: ListBuffer[A] = new ListBuffer[A]
}
