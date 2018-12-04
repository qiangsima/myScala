package scala.myCollection.immutable

import scala.myCollection.SeqFactory

trait Seq[+A] extends Iterable[A]
  with myCollection.Seq[A]
  with SeqOps[A, SeqOps, SeqOps[A]] {

  override def toSeq: this.type = this

}

trait SeqOps[+A, +CC[_], +C] extends Any with myCollection.SeqOps[A, CC, C]

object Seq extends SeqFactory.Delegate[Seq](List)

trait IndexedSeq[+A] extends Seq[A]
  with myCollection.IndexedSeq[A]
  with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]] {

}
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector)

trait IndexedSeqOps[A, +CC[_], +C]
  extends SeqOps[A, CC, C]
    with myCollection.IndexedSeqOps[A, CC, C] {

  override def slice(from: Int, until: Int): C = {
    if (from <= 0 && until >= length) coll
    else super.slice(from, until)
  }
}

trait LinearSeq[+A]
  extends Seq[A]
    with myCollection.LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]] {

  override def iterableFactory: SeqFactory[IterableCC] = LinearSeq
}

object LinearSeq extends SeqFactory.Delegate[LinearSeq](List)

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]]
  extends Any with SeqOps[A, CC, C]
    with myCollection.LinearSeqOps[A, CC, C]

abstract class AbstractSeq[+A] extends myCollection.AbstractSeq[A] with Seq[A]