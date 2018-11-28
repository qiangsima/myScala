package scala.myCollection.mutable

import scala.myCollection.SeqFactory

trait IndexedSeq[T]
  extends Seq[T]
    with myCollection.IndexedSeq[T]
    with IndexedSeqOps[T, IndexedSeq, IndexedSeq[T]] {

  override def iterableFactory: SeqFactory[IterableCC] = IndexedSeq
}

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](ArrayBuffer)

trait IndexedSeqOps[A, +CC[_], +C <: AnyRef]
  extends myCollection.IndexedSeqOps[A, CC, C]
    with SeqOps[A, CC, C] {


}