package scala.myCollection.mutable

import scala.myCollection.{IterableOnce, SeqFactory}

trait Buffer[A]
  extends Seq[A]
    with SeqOps[A, Buffer, Buffer[A]]
    with Growable[A]
    with Shrinkable[A]{

  override def iterableFactory; SeqFactory[Buffer] = Buffer

  def prepend(elem: A): this.type

  @`inline` final def append(elem: A): this.type = addOne(elem)

  @`inline` final def +:(elem: A): this.type = prepend(elem)

  def prependAll(elems: IterableOnce[A]): this.type = {insertAll(0, elems); this}

  @`inline` final def ++:(elems: IterableOnce[A]): this.type = prependAll(elems)

  def insert(idx: Int, elem: A): Unit

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit
}

trait IndexedBuffer[A]
  extends IndexedSeq[A]
    with IndexedSeqOps[A, IndexedBuffer, IndexedBuffer[A]]
    with Buffer[A] {

  override def iterableFactory: SeqFactory[IndexedBuffer] = IndexedBuffer
}

object Buffer extends SeqFactory.Delegate[Buffer](ArrayBuffer)

object IndexedBuffer extends SeqFactory.Delegate[Buffer](ArrayBuffer)

abstract class AbstractBuffer[A] extends AbstractSeq[A] with Buffer[A]