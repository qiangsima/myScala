package scala.myCollection.mutable

final class StringBuilder(val underlying: java.lang.StringBuilder)
  extends AbstractSeq[Char]
    with Builder[Char, String]
    with IndexedSeq[Char]
    with IndexedSeqOps[Char, IndexedSeq, StringBuilder]
    with java.lang.CharSequence{

  def this() = this(new java.lang.StringBuilder)

  def this(capacity: Int) = this(new java.lang.StringBuilder(capacity))

  def this(str: String) = this(new java.lang.StringBuilder(str))

  def this(initCapacity: Int, initValue: String) =
    this(new java.lang.StringBuilder(initValue.length + initCapacity) append initValue)

  def apply(n: Int): Char = underlying.charAt(n)

  def toCharArray: Array[Char] = {
    val len = underlying.length
    val arr = new Array[Char](len)
    underlying.getChars(0, len, arr, 0)
    arr
  }
}


object StringBuilder {
  def newBuilder = new StringBuilder
}