package scala.myCollection.mutable

class GrowableBuilder[Elem, To <: Growable[Elem]](protected val elems: To)
  extends Builder[Elem, To]{

  def clear(): Unit = elems.clear()

  def result() = elems

  def addOne(elem: Elem): this.type = {elems += elem; this}
}
