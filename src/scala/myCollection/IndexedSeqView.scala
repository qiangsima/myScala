package scala.myCollection

trait IndexedSeqView[+A] extends IndexedSeqOps[A, View, View[A]] with SeqView[A] { self =>

  override def view: IndexedSeqView[A] = this

}


object IndexedSeqView {

  type SomeIndexedSeqOps[A] = IndexedSeqOps[A, AnyConstr, _]

  class Id[+A](underlying: SomeIndexedSeqOps[A])
    extends SeqView.Id(underlying) with IndexedSeqView[A]
}