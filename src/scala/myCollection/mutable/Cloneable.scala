package scala.myCollection.mutable

trait Cloneable[+C <: AnyRef] extends scala.Cloneable {
  override def clone(): AnyRef = super.clone().asInstanceOf[C]
}
