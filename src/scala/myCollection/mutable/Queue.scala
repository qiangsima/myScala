package scala.myCollection.mutable

class Queue[A] protected (array: Array[AnyRef], start: Int, end: Int)
  extends ArrayDeque[A](array, start, end){

}
