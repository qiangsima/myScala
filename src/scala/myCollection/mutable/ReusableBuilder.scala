package scala.myCollection.mutable

trait ReusableBuilder [-Elem, +To] extends Builder[Elem, To] {

  override def clear(): Unit
  override def result(): To
}
