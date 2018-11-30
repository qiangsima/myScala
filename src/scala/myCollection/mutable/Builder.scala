package scala.myCollection.mutable

import scala.myCollection.IterableOnce


trait Builder[-A, +To] extends Growable[A] { self =>

  def clear(): Unit

  def result(): To

  def sizeHint(size: Int): Unit = ()

  final def sizeHint(coll: IterableOnce[_], delta: Int): Unit ={
    val s = coll.knownSize
    if (s != -1)sizeHint(delta + s)
  }

  final def sizeHintBounded(size: Int, boundingColl: myCollection.Iterable[_]): Unit = {
    if (boundingColl.knownSize != -1) {
      sizeHint(boundingColl.knownSize min size)
    }
  }

  def mapResult[NewTo](f: To => NewTo): Builder[A, NewTo] = new Builder[A, NewTo]{
    def addOne(x: A): this.type = {self += x; this}
    def clear(): Unit = self.clear()
    override def addAll(xs: IterableOnce[A]): this.type = {self ++= xs; this}
    override def sizeHint(size: Int): Unit = self.sizeHint(size)
    def result(): NewTo = f(self.result())
  }

}
