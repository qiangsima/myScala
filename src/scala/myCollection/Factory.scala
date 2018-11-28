package scala.myCollection

import scala.collection.mutable
import scala.myCollection.mutable.Builder
import scala.reflect.ClassTag

trait Factory[-A, +C] extends Any {

  def fromSpecific(it: IterableOnce[A]): C
  def newBuilder: Builder[A, C]
}

object Factory {
  implicit val stringFactory: Factory[Char, String] = new StringFactory

  private class StringFactory extends Factory[Char, String] with Serializable {
    def fromSpecific(it: IterableOnce[Char]): String = {
      val b = new mutable.StringBuilder(math.max(0, it.knownSize))
      b ++= it
      b.result()
    }
    def newBuilder: mutable.Builder[Char,String] = new StringBuilder()
  }

  implicit def arrayFactory[A: ClassTag]: Factory[A, Array[A]] = new ArrayFactory[A]

  private class ArrayFactory[A: ClassTag] extends Factory[A, Array[A]] {
    def fromSpecific(it: IterableOnce[A]): Array[A] = {
      val b = newBuilder
      b.sizeHint(math.max(0, it.knownSize))
      b ++= it
      b.result()
    }
    def newBuilder: mutable.Builder[A, Array[A]] = mutable.ArrayBuilder.make[A]
  }
}

trait IterableFactory[+CC[_]] extends Serializable {

  def from[A](source: IterableOnce[A]): CC[A]

  def empty[A]: CC[A]

  def apply[A](elems: A*): CC[A] = from(elems)

  def iterate[A](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len))(f)

  def newBuilder[A]: Builder[A, CC[A]]

  def fill[A](n: Int)(elem: => A): CC[A] = from(new View.Fill(n)(elem))

  implicit def iterableFactory[A]: Factory[A, CC[A]] = IterableFactory.toFactory(this)
}

object IterableFactory {
  implicit def toFactory[A, CC[_]](factory: IterableFactory[CC]): Factory[A, CC[A]] = new ToFactory[A, CC](factory)

  private[this] class ToFactory[A, CC[_]](factory: IterableFactory[CC]) extends Factory[A, CC[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): CC[A] = factory.from[A](it)
    def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]
  }

  implicit def toBuildFrom[A, CC[_]](factory: IterableFactory[CC]): BuildFrom[Any, A, CC[A]] =
    new BuildFrom[Any, A, CC[A]] {
      def fromSpecific(from: Any)(it: IterableOnce[A]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }
}

trait SeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends IterableFactory[CC] {

}

object SeqFactory {

  class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]]](delegate: SeqFactory[CC]) extends SeqFactory[CC]  {
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }

  final class UnapplySeqWrapper[A](private val c: SeqOps[A, Seq, Seq[A]]) extends AnyVal {
    def isEmpty = false
    def get = this
    def lengthCompare(len: Int): Int = c.lengthCompare(len)
    def apply(i: Int): A = c(i)
    def drop(n: Int): Seq[A] = c.view.drop(n).toSeq
    def toSeq: Seq[A] = c.toSeq
  }
}

trait StrictOptimizedSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends SeqFactory[CC] {

  override def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }
}