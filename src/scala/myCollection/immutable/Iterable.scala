package scala.myCollection.immutable

import scala.myCollection.IterableFactory

object Iterable extends IterableFactory.Delegate[Iterable](List)
