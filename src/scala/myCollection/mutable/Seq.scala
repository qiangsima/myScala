package scala.myCollection.mutable

import scala.myCollection.SeqFactory

trait Seq[A]
  extends Iterable[A]
    with myCollection.Seq[A]
    with SeqOps[A, Seq, Seq[A]] {

  override def iterableFactory: SeqFactory[IterableCC] = Seq
}

object Seq extends SeqFactory.Delegate[Seq](ArrayBuffer)

trait SeqOps[A, +CC[_], +C <: AnyRef]
  extends myCollection.SeqOps[A, CC, C]
    with Cloneable[C] {

  override def clone(): C = {
    val b = newSpecificBuilder
    b ++= toIterable
    b.result()
  }

  def update(idx: Int, elem: A): Unit
}

abstract class AbstractSeq[A] extends AbstractSeq[A] with Seq[A]
