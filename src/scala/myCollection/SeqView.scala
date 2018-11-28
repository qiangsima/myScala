package scala.myCollection

import scala.myCollection.View.SomeIterableOps

trait SeqView[+A] extends SeqOps[A, View, View[A]] with View[A] {

  override def view: SeqView[A] = this
  override def map[B](f: A => B): SeqView[B] = new SeqView.Map(this. f)
}

object SeqView {
  type SomeSeqOps[+A] = SeqOps[A, AnyConstr, _]

  class Id[+A](underlying: SeqOps[A, AnyConstr, _]) extends AbstractSeqView[A] {
    def apply(idx: Int): A = underlying.apply(idx)
    def length: Int = underlying.length
    def iterator: Iterator[A] = underlying.iterator
    override def knownSize: Int = underlying.knownSize
  }

  class Map[+A, +B](underlying: SomeSeqOps[A], f: A => B) extends View.Map[A, B](underlying, f) with SeqView[B] {
    def apply(idx: Int): B = f(underlying(idx))
    def length: Int = underlying.length
  }

  class Appended[+A](underlying: SomeSeqOps[A], elem: A) extends View.Appended(underlying, elem) with SeqView[A] {
    def apply(idx: Int): A = if (idx == underlying.length) elem else underlying(idx)
    def length: Int = underlying.length + 1
  }

  class Prepended[+A](elem: A, underlying: SomeSeqOps[A]) extends View.Prepended(elem, underlying) with SeqView[A] {
    def apply(idx: Int): A = if (idx == 0) elem else underlying(idx)
    def length: Int = underlying.length + 1
  }
}

abstract class AbstractSeqView[+A] extends AbstractView[A] with SeqView[A]