package scala.myCollection.immutable

import scala.myCollection.IterableFactory

trait Set[A] extends Iterable[A] with myCollection.Set[A] with SetOps[A, Set, Set[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Set
}
