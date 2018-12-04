package scala.myCollection

import scala.annotation.tailrec

trait LinearSeq[+A] extends Seq[A] with LinearSeqOps[A, LinearSeq, LinearSeq[A]] {
  override protected[this] def stringPrefix: String = "LinearSeq"
}

object LinearSeq extends SeqFactory.Delegate[LinearSeq](immutable.List)

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with LinearSeqOps[A, CC, C]] extends Any with SeqOps[A, CC, C] {
  def isEmpty: Boolean
  def head: A
  def tail: C

  def iterator: Iterator[A] =
    if (knownSize == 0) Iterator.empty
    else new LinearSeqIterator[A](this)


}

trait StrictOptimizedLinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with StrictOptimizedLinearSeqOps[A, CC, C]]
  extends LinearSeqOps[A, CC, C]
    with StrictOptimizedSeqOps[A, CC, C] {

  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var current: Iterable[A] = toIterable
    override def hasNext: Boolean = !current.isEmpty
    override def next(): A = {val r = current.head; current = current.tail; r}
  }

  override def drop(n: Int): C = {
    @tailrec def loop(n: Int, s: C): C =
      if (n <= 0 || s.isEmpty) s
      else loop(n-1, s.tail)
    loop(n, coll)
  }

  override def dropWhile(p: A => Boolean): C = {
    @tailrec def loop(s: C): C =
      if (!s.isEmpty && p(s.head)) loop(s.tail)
      else s
    loop(coll)
  }
}

private[myCollection] final class LinearSeqIterator[A](coll: LinearSeqOps[A, LinearSeq, LinearSeq[A]]) extends AbstractIterator[A] {

  private[this] final class LazyCell(st: => LinearSeqOps[A, LinearSeq, LinearSeq[A]]) { lazy val v = st}
  private[this] var these: LazyCell = new LazyCell(coll)
  def hasNext: Boolean = !these.v.isEmpty
  def next(): A =
    if (isEmpty) Iterator.empty.next()
    else {
      val cur = these.v
      val result = cur.head
      these = new LazyCell(cur.tail)
      result
    }
}
