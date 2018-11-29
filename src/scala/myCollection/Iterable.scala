package scala.myCollection

import scala.annotation.unchecked.uncheckedVariance
import scala.myCollection.View.PartitionWith
import scala.myCollection.mutable.Builder
import scala.myCollection.generic.DefaultSerializationProxy

trait Iterable[+A] extends IterableOnce[A] with IterableOps[A, Iterable, Iterable[A]] with Serializable {

  final def toIterable: this.type = this

  protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): IterableCC[A] @uncheckedVariance = iterableFactory.from(coll)
  protected def newSpecificBuilder: Builder[A, IterableCC[A]]@uncheckedVariance = iterableFactory.newBuilder[A]

  def iterableFactory: IterableFactory[IterableCC] = Iterable

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(iterableFactory.iterableFactory, this)

  protected[this] def className = stringPrefix

  private[scala] final def collectionClassName = className

  protected[this] def stringPrefix: String = "Iterable"

  override def toString: String = mkString(className + "(", ", ", ")")
}

trait IterableOps[+A, +CC[_], +C] extends Any with IterableOnce[A] with IterableOnceOps[A, CC, C] {

  protected type IterableCC[X] = CC[X] @uncheckedVariance

  def toIterable: Iterable[A]

  override def isTraversableAgain: Boolean = true

  protected def coll: C

  protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): C

  def iterableFactory: IterableFactory[IterableCC]

  protected def newSpecificBuilder: Builder[A @uncheckedVariance, C]

  def head: A = iterator.next()

  def headOption: Option[A] = {
    val it = iterator
    if (it.hasNext) Some(it.next()) else None
  }

  def last: A = {
    val it = iterator
    var lst = it.next()
    while (it.hasNext) lst = it.next()
    lst
  }

  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  def view: View[A] = View.fromIteratorProvider(() => iterator)

  def sizeCompare(otherSize: Int): Int = {
    if (otherSize < 0)1
    else {
      val known = knownSize
      if (known >= 0)Integer.compare(known, otherSize)
      else{
        var i = 0
        val it = iterator
        while (it.hasNext){
          if (i == otherSize) if (it.hasNext) 1 else 0
          it.next()
          i += 1
        }
        i - otherSize
      }
    }
  }

  def take(n: Int): C = fromSpecific(new View.Take(this, n))

  def map[B](f: A => B): CC[B] = iterableFactory.from(new View.Map(this, f))

  def flatMap[B](f: A => IterableOnce[B]): CC[B] = iterableFactory.from(new View.FlatMap(this, f))

  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B] = iterableFactory.from(new View.FlatMap(this, asIterable))

  def takeWhile(p: A => Boolean): C = fromSpecific(new View.TakeWhile(this, p))

  def collect[B](pf: PartialFunction[A, B]): CC[B] = iterableFactory.from(new View.Collect(this, pf))

  def partitionWith[A1, A2](f: A => Either[A1, A2]): (CC[A1], CC[A2]) = {
    val mp = new PartitionWith(this, f)
    (iterableFactory.from(mp.left), iterableFactory.from(mp.right))
  }

  def concat[B >: A](suffix: IterableOnce[B]): CC[B] = iterableFactory.from(suffix match {
    case xs: Iterable[B] => new View.Concat(this, xs)
    case xs => iterator ++ suffix.iterator
  })

  @`inline` final def ++[B >: A](suffix: IterableOnce[B]): CC[B] = concat(suffix)

  def tail: C = {
    if (isEmpty) throw new UnsupportedOperationException
    drop(1)
  }
}

object IterableOps {
  final class SizeCompareOps private[myCollection](val it: IterableOps[_, AnyConstr, _]) extends AnyVal {
    @inline def <(size: Int): Boolean = it.sizeCompare(size) < 0
  }

  class WithFilter[+A, +CC[_]] (self: IterableOps[A, CC, _],
                                p: A => Boolean) extends myCollection.WithFilter[A, CC] {
    protected def filtered: Iterable[A] =
      new View.Filter[A](self, p, isFlipped = false)

    def map[B](f: A => B): CC[B] =
      self.iterableFactory.from(new View.Map(filtered, f))

    override def flatMap[B](f: A => IterableOnce[B]): CC[B] =
      self.iterableFactory.from(new View.FlatMap(filtered, f))

    override def foreach[U](f: A => U): Unit = filtered.foreach(f)

    override def withFilter(q: A => Boolean): myCollection.WithFilter[A, CC] =
      new WithFilter(self, (a: A) => p(a) && q(a))
  }
}

object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable){

}

abstract class AbstractIterable[+A] extends Iterable[A]
