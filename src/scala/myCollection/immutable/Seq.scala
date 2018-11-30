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