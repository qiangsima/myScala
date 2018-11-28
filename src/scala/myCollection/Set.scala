package scala.myCollection

import scala.language.higherKinds
import scala.util.hashing.MurmurHash3

trait Set[A]
  extends Iterable[A]
  with SetOps[A, Set, Set[A]]
  with Equals {

  def canEqual(that: Any) = true

  override def equals(that: Any): Boolean = {
    that match {
      case set: Set[A] =>
        (this eq set) ||
          (set canEqual this) &&
            (toIterable.size == set.size) &&
            (this subsetOf set)
      case _ => false
    }
  }

  override def hashCode(): Int = MurmurHash3.setHash(toIterable)

  override def iterableFactory: IterableFactory[IterableCC] = Set

  def empty: IterableCC[A] = iterableFactory.empty

  override protected[this] def stringPrefix: String = "Set"

  override def toString(): String = super[Iterable].toString()
}

trait SetOps[A, +CC[_], +C <: SetOps[A, CC, C]]
  extends IterableOps[A, CC, C]
  with (A => Boolean) {

  def contains(elem: A): Boolean

  def subsetOf(that: Set[A]): Boolean = this.forall(that)

  def subset(len: Int): Iterator[C] = {
    if (len < 0 || len > size)Iterator.empty
    else new SubsetsItr(toIterable.to(IndexedSeq), len)
  }

  private class SubsetsItr(elms: IndexedSeq[A], len: Int) extends AbstractIterator[C] {
    private[this] val idxs = Array.range(0, len+1)
    private[this] var _hasNext = true
    idxs(len) = elms.size

    def hasNext = _hasNext

    def next(): C = {
      if (!hasNext) Iterator.empty.next()

      var buf = newSpecificBuilder
      idxs.slice(0, len) foreach (idx => buf += elms(idx))
      val result = buf.result()

      var i = len - 1
      while (i >= 0 && idxs(i) == idxs(i+1)-1) i -= 1

      if (i < 0) _hasNext = false
      else {
        idxs(i) += 1
        for (j <- (i+1) until len)
          idxs(j) = idxs(j-1) + 1
      }

      result
    }
  }

  def intersect(that: Set[A]): C = this.filter(that)

  /** Alias for `intersect` */
  @`inline` final def &(that: Set[A]): C = intersect(that)

  def diff(that: Set[A]): C

  @`inline` final def &~ (that: Set[A]): C = this diff that

  def concat(that: collection.Iterable[A]): C = fromSpecific(that match {
    case that: collection.Iterable[A] => new View.Concat(toIterable, that)
    case _ => iterator.concate(that.iterator)
  })

  @`inline` final def ++ (that: collection.Iterable[A]): C = concat(that)

  def empty: C
}

object Set extends IterableFactory.Delegate[Set](immutable.Set)

abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]