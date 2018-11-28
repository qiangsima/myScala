package scala.myCollection.generic

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.myCollection.{Factory, Iterable}
import scala.myCollection.mutable.Builder

final class DefaultSerializationProxy[A](factory: Factory[A, Any], @transient private[this] val coll: Iterable[A]) extends Serializable {

  @transient protected var builder: Builder[A, Any] = _

  private[this] def writeObject(out: ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    val k = coll.knownSize
    out.writeInt(k)
    var count = 0
    coll foreach { x =>
      out.writeObject(x)
      count += 1
    }
    if (k >= 0) {
      if (count != k) throw new IllegalStateException(s"Illegal size $count of collection, excepted $k")
    } else out.writeObject(SerializeEnd)
  }

  private[this] def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    builder = factory.newBuilder
    val k = in.readInt()
    if (k > 0){
      builder.sizeHint(k)
      var count = 0
      while (count < k){
        builder += in.readObject().asInstanceOf[A]
        count += 1
      }
    } else {
      while (true) in.readObject match {
        case SerializeEnd => return
        case a => builder += a.asInstanceOf[A]
      }
    }
  }

  protected[this] def readResolve(): Any = builder.result()
}

private[myCollection] case object SerializeEnd
