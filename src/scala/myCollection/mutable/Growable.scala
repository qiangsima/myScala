package scala.myCollection.mutable

import scala.myCollection.IterableOnce

trait Growable[-A] extends Clearable {

  def addOne(elem: A): this.type

  @`inline` final def += (elem: A) = addOne(elem)

  def addAll(xs: IterableOnce[A]): this.type = {
    val it = xs.iterator
    while (it.hasNext)addOne(it.next())
    this
  }

  @`inline` final def ++= (xs: IterableOnce[A]) = addAll(xs)

}

object Growable {

  def from[A](empty: Growable[A], it: IterableOnce[A]): empty.type = empty ++= it

}

trait Clearable {
  def clear(): Unit
}
