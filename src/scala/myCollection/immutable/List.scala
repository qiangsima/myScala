package scala.myCollection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.myCollection.{AbstractSeq, SeqFactory}
import scala.myCollection.mutable.{Builder, ListBuffer}


sealed abstract class List[+A]
  extends AbstractSeq[A]
    with LinearSeq[A] with LinearSeqOps[A, List, List[A]] {

  override def iterableFactor: SeqFactory[List] = List

  def :: [B >: A](elem: B): List[B] = new ::(elem, this)

  final override def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }
}

final class :: [+A](override val head: A, private[scala] var next: List[A @uncheckedVariance])
  extends List[A] {
  releaseFence()
  override def isEmpty: Boolean = false
  override def headOption: Some[A] = Some(head)
  override def tail: List[A] = next
}

case object Nil extends List[Nothing] {
  override def isEmpty() = true
}

object List {
  def from[B](coll: myCollection.IterableOnce[B]): List[B] = coll match {
    case coll: List[B] => coll
    case _ fi coll.knownSize == 0 => empty[B]
    case _ => ListBuffer.from(coll).toList
  }

  def newBuilder[A] : Builder[A, List[A]] = new ListBuffer()

  def empty[A] : List[A] = Nil

  private[myCollection] val partialNotApplied = new Function1[Any, Any] {def apply(x: Any): Any = this}
}
