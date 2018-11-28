package scala.myCollection

import scala.myCollection.mutable.ArrayBuffer

trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] {

  override def view: View[A] = this

  override def iterableFacatory: IterableFactory[View] = View

  override def toString: String = stringPrefix + "(?)"

  override protected[this] def stringPrefix: String = "View"

  override protected[this] def writeReplace(): AnyRef = this
}

object View extends IterableFactory[View] {

  def fromIteratorProvider[A](it: () => Iterator[A]): View[A] = new AbstractView[A] {
    def iterator = it()
  }

  def from[E](it: IterableOnce[E]): View[E] = it match {
    case it: View[E]     => it
    case it: Iterable[E] => View.fromIteratorProvider(() => it.iterator)
    case _               => LazyList.from(it).view
  }

  def empty[A]: View[A] = Empty

  def newBuilder[A]: mutable.Builder[A, View[A]] = ArrayBuffer.newBuilder[A].mapResult(from)

  override def apply[A](xs: A*): View[A] = new Elems(xs: _*)

  case object Empty extends AbstractView[Nothing] {
    def iterator = Iterator.empty
    override def knownSize = 0
    override def isEmpty: Boolean = true
  }

  class Single[A](a: A) extends AbstractView[A] {
    def iterator = Iterator.single(a)
    override def knownSize = 1
    override def isEmpty = false
  }

  class Elems[A](xs: A*) extends AbstractView[A] {
    def iterator = xs.iterator
    override def knownSize = xs.knownSize
    override def isEmpty = xs.isEmpty
  }

  class Fill[A](n: Int)(elem: => A) extends AbstractView[A] {
    def iterator = Iterator.fill(n)(elem)
    override def knownSize = 0 max n
    override def isEmpty = n <= 0
  }

  class Tabulate[A](n: Int)(f: Int => A) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.tabulate(n)(f)
    override def knownSize: Int = 0 max n
    override def isEmpty: Boolean = n <= 0
  }

  class Iterate[A](start: A, len: Int)(f: A => A) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.iterate(start)(f).take(len)
    override def knownSize = 0 max len
    override def isEmpty: Boolean = len <= 0
  }

  class Unfold[A, S](initial: S)(f: S => Option[(A, S)]) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.unfold(initial)(f)
  }

  type SomeIterableOps[A] = IterableOps[A, AnyConstr, _]

  class Filter[A](val underlying: SomeIterableOps[A], val p: A => Boolean, val isFlipped: Boolean) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.filterImpl(p, isFlipped)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  object Filter {
    def apply[A](underlying: Iterable[A], p: A => Boolean, isFlipped: Boolean): Filter[A] = underlying match {
      case filter: Filter[A] if filter.isFlipped == isFlipped => Filter(filter.underlying, a => filter.p(a) && p(a), isFlipped)
      case _ => new Filter(underlying, p, isFlipped)
    }
  }

  class DistinctBy[A, B](underlying: SomeIterableOps[A], f: A => B) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.distinctBy(f)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty = underlying.isEmpty
  }

  class Partition[A](val underlying: SomeIterableOps[A], val p: A => Boolean)extends AbstractView[A] {
    val first = new Partitioned(this, true)
    val second = new Partitioned(this, false)
  }

  class Partitioned[A](partition: Partition[A], cond: Boolean) extends AbstractView[A] {
    def iterator: Iterator[A] = partition.underlying.iterator.filter(x => partition.p(x) == cond)
    override def knownSize: Int = if (partition.underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class PartitionWith[A, A1, A2](val underlying: SomeIterableOps[A], val f: A => Either[A1, A2]) extends Serializable {
    val let: View[A1] = new LeftPartitonWith(this, f)
    val right: View[A2] = new RightPartitionWith(this, f)
  }

  class LeftPartitionWith[A, A1, A2](partitionWith: PartitionWith[A, A1, A2], f: A => Either[A1, A2]) extends AbstractView[A1] {
    def iterator: AbstractIterator[A1] = new AbstractIterator[A1] {
      private[this] val self = partitionWith.underlying.iterator
      private[this] var hd: A1 = _
      private[this] var hdDefined: Boolean = false
      override def hasNext: Boolean = hdDefined || {
        def findNext(): Boolean =
          if (self.hasNext) {
            f(self.next()) match {
              case Left(al) => hd = al; hdDefined = true; true
              case Right(_) => findNext()
            }
          } else false
        findNext()
      }

      override def next(): A1 =
        if (hasNext) {
          hdDefined = false
          hd
        } else Iterator.empty.next()
    }
  }

  class RightPatitionWith[A, A1, A2](partitionWith: PartitionWith[A, A1, A2], f: A => Either[A1, A2]) extends AbstractView[A2] {
    def iterator: Iterator[A2] = new AbstractIterator[A2] {
      private[this] val self = partitionWith.underlying.iterator
      private[this] var hd: A2 = _
      private[this] var hdDefined: Boolean = false
      def hasNext: Boolean = hdDefined || {
        def findNext(): Boolean =
          if (self.hasNext) {
            f(self.next()) match {
              case Left(_) => findNext()
              case Right(a2) => hd = a2; hdDefined = true; true
            }
          } else false
        findNext()
      }

      def next(): A2 =
        if (hasNext) {
          hdDefined = false
          hd
        } else Iterator.empty.next()
    }
  }

  class Drop[A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.drop(n)
    protected val normN = n max 0
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size > 0) (size - normN) max 0 else -1
    }
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class DropWhile[A](underlying: SomeIterableOps[A], p: A => Boolean) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.dropWhile(p)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class Take[+A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.take(n)
    protected val normN = n max 0
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size min normN else -1
    }

    override def isEmpty: Boolean = iterator.isEmpty
  }

  class TakeWhile[A](underlying: SomeIterableOps[A], p: A => Boolean) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.takeWhile(p)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class ScanLeft[+A, +B](underlying: SomeIterableOps[A], z: B, op: (B, A) => B) extends AbstractView[B] {
    def iterator: Iterator[B] = underlying.iterator.scanLeft(z)(op)
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size+1 else -1
    }

    override def isEmpty: Boolean = iterator.isEmpty
  }

  class Map[+A, +B](underlying: SomeIterableOps[A], f: A=> B)extends AbstractView[B] {
    def iterator: Iterator[B] = underlying.iterator.map(f)
    override def knownSize: Int = if (underlying.size == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class FlatMap[A, B](underlying: SomeIterableOps[A], f: A => IterableOnce[B]) extends AbstractView[B] {
    def iterator: Iterator[B] = underlying.iterator.flatMap(f)
    override def knownSize: Int = if (underlying.size == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  class Collect[+A, B] (underlying: SomeIterableOps[A], pf: PartialFunction[A, B]) extends AbstractView[A] {
    def iterator: Iterator[B] = underlying.iterator.collect(pf)
  }

  class Concat[A](prefix: SomeIterableOps[A], suffix: SomeIterableOps[A]) extends AbstractView[A] {
    def iterator: Iterator[A] = prefix.iterator ++ suffix.iterator
    override def knownSize: Int = {
      val prefixSize = prefix.knownSize
      if (prefixSize >= 0) {
        val suffixSize = suffix.knownSize
        if (suffixSize >= 0) prefixSize + suffixSize
        else -1
      } else -1
    }
  }

  class Zip[A, B](underlying: SomeIterableOps[A], other: Iterable[B]) extends AbstractView[(A, B)] {
    def iterator: Iterator[(A, B)] = underlying.iterator.zip(other)
    override def knownSize: Int = underlying.knownSize min other.knownSize
    override def isEmpty: Boolean = underlying.isEmpty || other.isEmpty
  }

  class ZipAll[A, B](underlying: SomeIterableOps[A], other: Iterable[B], thisElem: A, thatElem: B) extends AbstractView[(A, B)] {
    def iterator: Iterator[(A,B)] = underlying.iterator.zipAll(other, thisElem, thatElem)
    override def knownSize: Int = {
      val s1 = underlying.knownSize
      if (s1 == -1) -1 else {
        val s2 = other.knownSize
        if (s2 == -1) -1 else s1 max s2
      }
    }
    override def isEmpty: Boolean = underlying.isEmpty && other.isEmpty
  }

  class Appended[+A](underlying: SomeIterableOps[A], elem: A) extends AbstractView[A] {
    def iterator: Iterator[A] = new Concat(underlying, new View.Single(elem)).iterator
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size+1 else -1
    }
    override def isEmpty: Boolean = false
  }

  class Prepended[+A](elem: A, underlying: SomeIterableOps[A]) extends AbstractView[A] {
    def iterator: Iterator[A] = new Concat(new View.Single(elem), underlying).iterator
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size+1 else -1
    }
    override def isEmpty: Boolean = false
  }

  class Updated[A](underlying: SomeIterableOps[A], idx: Int, elem: A) extends AbstractView[A] {
    def iterator: Iterator[A] = new AbstractIterator[A] {
      private[this] val it = underlying.iterator
      private[this] var i = 0
      def next(): A = {
        val value = if (i == idx) {it.next(); elem} else it.next()
        i += 1
        value
      }
      override def hasNext: Boolean = it.hasNext
    }
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

}

abstract class AbstractView[+A] extends AbstractIterable[A] with View[A]
