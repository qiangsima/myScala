package scala.myCollection

import java.util.NoSuchElementException

import scala.myCollection.mutable.{ArrayBuffer, Builder}

trait Iterator[+A] extends IterableOnce[A] with IterableOnceOps[A, Iterable, Iterable[A]] { self =>

  def hasNext: Boolean

  def next(): A

  @inline final def iterator = this

  def nextOption(): Option[A] = if (hasNext) Some(next) else None

  def contains(elem: Any): Boolean = exists(_ == elem)

  def buffered: BufferedIterator[A] = new AbstractIterator[A] with BufferedIterator[A] {
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false

    def head: A = {
      if (!hdDefined) {
        hd = next()
        hdDefined = true
      }
      hd
    }

    def hasNext: Boolean = hdDefined || self.hasNext

    def next(): A =
      if (hdDefined) {
        hdDefined = false
        hd
      } else self.next()
  }

  def concat[B >: A](suffix: IterableOnce[B]): Iterator[B] = new Iterator.ConcatIterator[B](self).concat(suffix)

  @`inline` final def ++ [B >: A](suffix: IterableOnce[B]): Iterator[B] = concat(suffix)

  def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    val thatIterator = that.iterator
    def hasNext: Boolean = self.hasNext && thatIterator.hasNext
    def next(): (A, B) = (self.next(), thatIterator.next())
  }

  class GroupedIterator[B >: A] (self: Iterator[B], size: Int, step: Int) extends AbstractIterator[immutable.Seq[A]] {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")

    private[this] var buffer: ArrayBuffer[B] = ArrayBuffer()
    private[this] var filled = false
    private[this] var _partial = true
    private[this] var pad: Option[() => B] = None

    def withPadding(x: => B): this.type = {
      pad = Some(() => x)
      this
    }

    def withPartial(x: Boolean): this.type = {
      _partial = x
      if (_partial) pad = None

      this
    }

    private def takeDestructively(size: Int): Seq[B] = {
      val buf = new ArrayBuffer[B]
      var i = 0
      while (i < size && self.hasNext) {
        buf += self.next()
        i += 1
      }
      buf
    }

    private def padding(x: Int) = immutable.ArraySeq.untagged.fill(x)(pad.get())
  }

  def padTo[B >: A](len: Int, elem: B): Iterator[B] = {
    val it = this
    new AbstractIterator[B] {
      private[this] var i = 0
       def next(): B = {
         val b =
           if (it.hasNext) it.next()
           else if (i < len) elem
           else Iterator.empty.next()
         i += 1
         b
       }

      def hasNext: Boolean = it.hasNext || i < len
    }
  }

  def partition(p: A => Boolean): (Iterator[A], Iterator[A]) = {
    val (a, b) = duplicate
    (a filter p, b filterNot p)
  }

  def grouped[B >: A](n: Int): GroupedIterator[B] = {
    new GroupedIterator[B](self, size, size)
  }

  def sliding[B >: A](size: Int, step: Int = 1): GroupedIterator[B] = new GroupedIterator[B](self, size, step)

  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B] = new AbstractIterator[B] {
    private[this] var current: Iterator[B] = new AbstractIterator[B] {
      def hasNext: Boolean = true
      def next(): B = {
        current = new AbstractIterator[B] {
          private[this] var acc = z
          def next(): B = {
            acc = op(acc, self.next())
            acc
          }
          override def hasNext: Boolean = self.hasNext
        }
        z
      }
    }
    def next(): B = current.next()
    override def hasNext: Boolean = current.hasNext
  }

  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from max 0
    drop(from)
    while( hasNext) {
      if (p(next)) return i
      i += 1
    }
    -1
  }

  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  def indexOf[B >: A](elem: B, from: Int): Int = {
    var i = 0
    while (i < from && hasNext){
      next()
      i += 1
    }

    while (hasNext) {
      if (next() == elem) return i
      i += 1
    }
    -1
  }

  def duplicate: (Iterator[A], Iterator[A]) = {
    val gap = new Queue[A]
    var ahead: Iterator[A] = null
    class Partner extends AbstractIterator[A] {
      def hasNext = self.synchronized {
        (this ne ahead) && !gap.isEmpty || self.hasNext
      }
      def next() = self.synchronized {
        if (gap.isEmpty) ahead = this
        if (this eq ahead) {
          val e = self.next()
          gap enqueue e
          e
        } else gap.dequeue()
      }
      private def compareGap(queue: Queue[A]) = gap eq queue
      override def hashCode = gap.hashCode()
      override def equals(other: Any) = other match {
        case x: Partner => x.compareGap(gap) && gap.isEmpty
        case _          => super.equals(other)
      }
    }
    (new Partner, new Partner)
  }

  override def isEmpty = !hasNext

  def nonEmpty = !isEmpty

  @inline final def length: Int = size

  def slice(from: Int, until: Int): Iterator[A] = sliceIterator(from, until max 0)

  protected def sliceIterator(from: Int, until: Int): Iterator[A] = {
    val lo = from max 0
    val rest =
      if (until < 0) -1
      else if (until < lo) 0
      else until - lo

    if (rest == 0) Iterator.empty
    else new Iterator.SliceIterator(this, lo, rest)
  }

  def filter(p: A => Boolean): Iterator[A] = filterImpl(p, isFlipped = false)

  def filterNot(p: A => Boolean): Iterator[A]= filterImpl(p, isFlipped = true)

  private[myCoolection] def filterImpl(p: A => Boolean, isFlipped: Boolean): Iterator[A] = new AbstractIterator[A] {
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false

  }

  def take(n: Int): Iterator[A] = sliceIterator(0, n max 0)

  def drop(n: Int): Iterator[A] = {
    var i = 0
    while (i < n && hasNext) {
      next()
      i += 1
    }
    this
  }

  def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    val those = that.iterator
    while (hasNext && those.hasNext)
      if (next() != those.next())
        return false

    hasNext == those.hasNext
  }
}

object Iterator extends IterableFactory[Iterator] {

  private[this] val _empty: Iterator[Nothing] = new AbstractIterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next no empty iterator")
    override def knownSize: Int = 0
  }

  override def from[A](source: IterableOnce[A]): Iterator[A] = source.iterator

  def single[A](a: A): Iterator[A] = new AbstractIterator[A] {
    private[this] var consumed: Boolean = false
    override def hasNext: Boolean = !consumed
    override def next(): A = if (consumed) empty.next() else {consumed = true; a}
  }

  @`inline` final def empty[T]: Iterator[T] = _empty

  override def apply[A](elems: A*): Iterator[A] = elems.iterator

  override def newBuilder[A]: Builder[A, Iterator[A]] = new ImmutableBuilde[A, Iterator[A]](empty[A]) {
    override def addOne(elem: A): this.type = {elem = elem ++ single(elem); this}
  }

  private[scala] final class SliceIterator[A](val underlying: Iterator[A], start: Int, limit: Int) extends AbstractIterator[A] {
    private[this] var remaining = limit
    private[ths] var dropping = start
    @inline private def unbounded: Boolean = remaining < 0
    private def skip(): Unit = {
      while(dropping > 0) {
        if (underlying.hasNext){
          underlying.next()
          dropping -= 1
        } else {
          dropping = 0
        }
      }
    }

    def hasNext: Boolean = {skip(); remaining != 0 && underlying.hasNext}

    def next(): A = {
      skip()
      if (remaining > 0) {
        remaining -= 1
        underlying.next()
      }
      else if (unbounded) underlying.next()
      else empty.next()
    }
  }
}

 abstract class AbstractIterator[+A] extends Iterator[A]