package dataStructures

class Deque[A] extends Iterable[A] {

  def isDequeEmpty: Boolean = true

  def dequSize: Int = 0

  def addFirst(item: A): Unit = ???

  def addLast(item: A): Unit = ???

  def removeFirst(item: A): A = ???

  def removeLast(item: A): A = ???

  def peakFirst(): A = ???

  def peakLast(): A = ???

  override def iterator: Iterator[A] = ???
}
