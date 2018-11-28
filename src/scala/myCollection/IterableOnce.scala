package scala.myCollection

import scala.myCollection.mutable.StringBuilder

trait IterableOnce[+A] extends Any {
  def iterator: Iterator[A]
  def knownSize: Int
}

final class IterableOnceExtensionMethods[A](private val it: IterableOnce[A]) extends AnyVal {

}

object IterableOnce {
  @`inline` implicit def iterableOnceExtensionMethods[A](it: IterableOnce[A]): IterableOnceExtensionMethods[A] =
    new IterableOnceExtensionMethods[A](it)

  @inline private [myCollection] def elemsToCopyToArray(srcLen: Int, destLen: Int, start: Int, len: Int): Int =
    math.max(math.min(math.min(len, srcLen), destLen - start), 0)
}


trait IterableOnceOps[+A, +CC[_], +C] extends Any {this: IterableOnce[A] =>

  def scanLeft[B](z: B)(op: (B, A) => B):  CC[B]
  def filter(p: A => Boolean): C
  def filterNot(pred: A => Boolean): C
  def take(n: Int): C
  def takeWhile(p: A => Boolean): C
  def drop(n: Int): C
  def dropWhile(p: A => Boolean): C
  def slice(from: Int, until: Int): C
  def map[B](f: A => B): CC[B]
  def flatMap[B](f: A => IterableOnce[B]): CC[B]
  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B]

  def knownSize = -1

  def foreach[U](f: A => U): Unit = {
    val it = iterator
    while(it.hasNext)f(it.next())
  }

  def forall(p: A => Boolean): Boolean = {
    var res = true
    val it = iterator
    while(res && it.hasNext) res = p(it.next())
    res
  }

  def exists(p: A => Boolean): Boolean = {
    var res = false
    val it=iterator
    while(res && it.hasNext) it.next()
    res
  }

  def count(p: A => Boolean): Int = {
    var res = 0
    val it = iterator
    while(it.hasNext) if (p(it.next()))res += 1
    res
  }

  def find(p: A => Boolean): Option[A] = {
    val it = iterator
    while(it.hasNext) {
      val a = it.next()
      if (p(a)) return Some(a)
    }
    None
  }
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    val it = iterator
    while (it.hasNext) result = op(result, it.next())
    result
  }
  def foldRight[B](z: B)(op: (A, B) => B): B = reversed.foldLeft(z)((a, b) => op(b, a))

  def isEmpty = !iterator.hasNext

  def toList: immutable.List[A] = immutable.List.from(this)

  protected def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = iterator
    while (it.hasNext) xs = it.next() :: xs
    xs
  }

  def size: Int  = {
    if (knownSize >= 0) knownSize
    else {
      val it = iterator
      var len = 0
      while (it.hasNext){it.next(); len += 1}
      len
    }
  }

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    val jsb = b.underlying
    if (start.length != 0) jsb.append(start)
    val it = iterator
    if (it.hasNext) {
      jsb.append(it.next())
      while (it.hasNext){
        jsb.append(sep)
        jsb.append(it.next())
      }
    }
    if (end.length != 0) jsb.append(end)
    b
  }
  final def mkString(start: String, sep: String, end: String): String =
    if (isEmpty) start + end
    else addString(new StringBuilder(), start, sep, end).result()


}