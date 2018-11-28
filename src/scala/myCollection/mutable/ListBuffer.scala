package scala.myCollection.mutable

import scala.myCollection.IterableOnce

class ListBuffer[A]
  extends AbstractBuffer[A]
    with SeqOps[A, ListBuffer, ListBuffer[A]] {

  private var first: List[A] = Nil
  private var last0 ::[A] = null

}

object ListBuffer {

  def from[A](coll: IterableOnce[A]): ListBuffer[A] = new ListBuffer[A] ++= coll

  def newBuilder[A] : Builder[A, ListBuffer[A]] = new GrowableBuilder(empty[A])

  def empty[A]: ListBuffer[A] = new ListBuffer[A]
}
