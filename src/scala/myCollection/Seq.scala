package scala.myCollection

import scala.util.hashing.MurmurHash3

trait Seq[+A]
  extends Iterable[A]
    with PartialFunction[Int, A]
    with SeqOps[A, Seq, Seq[A]]
    with Equals{

  override def iterableFactory: IterableFactory[IterableCC] = Seq

  def canEqual(that: Any) = true

  override def equals(o: Any): Boolean = this.eq(o.asInstanceOf[AnyRef]) || (o match {
    case it: Seq[A] => (it eq this) || (it canEqual this) && sameElements(it)
    case _ => false
  })

  override def hashCode(): Int  = MurmurHash3.seqHash(toIterable)

  override def toString() = super[Iterable].toString()

  override protected[this] def stringPrefix: String = "Seq"
}

object Seq extends SeqFactory.Delegate[Seq](immutable.Seq)

trait SeqOps[+A, +CC[_], +C] extends Any
  with IterableOps[A, CC, C] { self =>

  override def view: SeqView[A] = new SeqView.Id[A](this)

  def apply(i: Int): A

  def length: Int

  def prepended[B >: A](elem: B): CC[B] = iterableFactory.from(new View.Prepended(elem, this))

  @`inline` final def +: [B >: A](elem: B): CC[B] = prepended(elem)

  def appended[B >: A](elem: B): CC[B] = iterableFactory.from(new View.Appended(this, elem))

  @`inline` final def :+ [B >: A](elem: B): CC[B] = appended(elem)

  def prependedAll[B >: A](prefix: IterableOnce[B]): CC[B] = iterableFactory.from(prefix match {
    case prefix: Iterable[B] => new View.Concat(prefix, this)
    case _ => prefix.iterator ++ iterator
  })
}

abstract class AbstractSeq[+A] extends AbstractIterable[A] with Seq[A]