package scala.myCollection

abstract class WithFilter[+A, +CC[_]] {

  def map[B](f: A => B): CC[B]
  def flatMap[B](f: A => IterableOnce[B]): CC[B]
  def foreach[U](f: A => U): Unit
  def withFilter(q: A => Boolean): WithFilter[A, CC]
}
