package scala.myCollection.mutable

import scala.myCollection.StrictOptimizedSeqFactory

class ArrayBuffer[A] private (initialElements: Array[AnyRef], initialSize: Int)
  extends AbstractBuffer[A]
    with IndexedBuffer[A]
    with IndexedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with StrictOptimizedSeqOps[A, ArrayBuffer, ArrayBuffer[A]] {

  def this() = this(new Array[AnyRef](16), 0)
}


object ArrayBuffer extends StrictOptimizedSeqFactory[ArrayBuffer] {

}