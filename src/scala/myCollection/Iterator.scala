package scala.myCollection

trait Iterator[+A] extends IterableOnce[A] with IterableOnceOps[A, Iterable, Iterable[A]] { self =>

  def hasNext: Boolean

  def next(): A

  @inline final def iterator = this

  def nextOption(): Option[A] = if (hasNext) Some(next) else None

  def contains(elem: Any): Boolean = exists(_ == elem)

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


}

 abstract class AbstractIterator[+A] extends Iterator[A]