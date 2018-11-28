package scala.myCollection

trait IndexedSeq[+A] extends Seq[A] with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]] {

  override protected[this] def stringPrefix = "IndexedSeq"
}

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

trait IndexedSeqOps[+A, +CC[_], +C] extends Any with SeqOps[A, CC, C] { self =>

  def iterator: Iterator[A] = view.iterator

  override def reverseIterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var i = self.length
    def hasNext: Boolean = 0 < i
    def next(): A = {
      if (0 < i){
        i -= 1
        self(i)
      } else Iterator.empty.next()
    }
  }

  override def view: IndexedSeqView[A] = new IndexedSeqView.Id[A](this)
}